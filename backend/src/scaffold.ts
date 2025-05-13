import * as z from "@npm/zod";
import { getCookies } from "@std/http";
import { serveFile } from "@std/http/file-server";
import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task";
import { setAuthentication } from "./auth.ts"
import { getPriorities, updatePriorities } from "./priorities.ts"
import { getAllWaypoints, deleteWaypoint, updateWaypoint } from "./waypoints.ts"
// @ts-types="./elm/main.d.ts"
import { Elm } from "./elm/main.js"

function makeMatchRoute(url: string) {
  return function<T extends {[index:string]: string} = {}>(pattern: string) {
    const matched = new URLPattern({ pathname: pattern }).exec(url);
    if (!matched) {
      return null;
    }
    const parsed =
      z.object({
        pathname: z.object({
          groups: z.object({}).catchall(z.string().transform(decodeURIComponent))
        })
      })
        .safeParse(matched);
    if (!parsed.success) {
      return null;
    }
    return parsed.data.pathname.groups as T;
  };
}

async function handle(req: Request) {
  const matchRoute = makeMatchRoute(req.url);
  const route_api_login =
    matchRoute("/-/api/login");
  const route_api_priorities =
    matchRoute("/-/api/priorities");
  const route_api_waypoints =
    matchRoute("/-/api/waypoints");
  const route_api_waypoints_id =
    matchRoute<{ id: string }>("/-/api/waypoints/:id");
  if (req.method == "POST" && route_api_login) {
    const body =
      z.object({
        token: z.string()
      })
        .strict()
        .safeParse(await req.json());
    if (!body.success) {
      return new Response(null, { status: 400 });
    }
    const headers = new Headers();
    setAuthentication(headers, body.data.token);
    return new Response(null, { headers });
  } else if (req.method == "GET" && route_api_priorities) {
    const kv = await Deno.openKv();
    const priorities = await getPriorities(kv);
    await kv.close()
    return new Response(JSON.stringify(priorities));
  } else if (req.method == "PATCH" && route_api_priorities) {
    const body =
      z.object({
        priorities: z.string().array().optional()
      })
        .strict()
        .safeParse(await req.json());
    if (!body.success) {
      return new Response(null, { status: 400 });
    }
    const kv = await Deno.openKv();
    await updatePriorities(kv, body.data);
    await kv.close()
    return new Response("");
  } else if (req.method == "GET" && route_api_waypoints) {
    const kv = await Deno.openKv();
    const waypoints = await getAllWaypoints(kv);
    await kv.close()
    return new Response(JSON.stringify({ waypoints }));
  } else if (req.method == "DELETE" && route_api_waypoints_id) {
    const kv = await Deno.openKv();
    await deleteWaypoint(kv, route_api_waypoints_id.id);
    await kv.close()
    return new Response("");
  } else if (req.method == "PATCH" && route_api_waypoints_id) {
    const body =
      z.object({
        text: z.string().optional(),
        completed: z.boolean().optional(),
        url: z.string().url().nullable().optional(),
        requires: z.string().array().optional(),
        requiredBy: z.string().array().optional(),
        source: z.string().optional(),
      })
        .strict()
        .safeParse(await req.json());
    if (!body.success) {
      return new Response(null, { status: 400 });
    }
    const kv = await Deno.openKv();
    await updateWaypoint(kv, route_api_waypoints_id.id, body.data);
    await kv.close()
    return new Response("");
  }
  return new Response(null, { status: 404 });
}

const app = Elm.Backend.Main.init();

ConcurrentTask.register({
  tasks: {
    "env:get": (key: string) => Deno.env.get(key) ?? null,
    "resp:file": ({ request, filename }: { request: Request, filename: string }) => serveFile(request, filename),
    "resp:get": ({ status, body }: { status: number, body: string }) => new Response(body, { status }),
    "resp:legacy": handle,
    "req:getMethod": (request: Request) => request.method,
    "req:getUrl": (request: Request) => request.url,
    "req:getCookie": ({ request, key }: { request: Request, key: string}) => getCookies(request.headers)[key] ?? null,
  },
  ports: {
    send: app.ports.taskRequests,
    receive: app.ports.taskResponses,
  },
});

app.ports.responses.subscribe(({ response, resolver, message }) => {
  if (message !== null) {
    console.log(message)
  }
  resolver(response)
});

app.ports.errors.subscribe((message) => {
  throw new Error(message)
});

Deno.serve(async (request) => {
  return await new Promise((resolve) => {
    app.ports.requests.send({ request, resolver: resolve });
  });
});
