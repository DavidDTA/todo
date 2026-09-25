import * as z from "@npm/zod";
import { getCookies } from "@std/http";
import { serveFile } from "@std/http/file-server";
import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task";
import { randomBytes } from "node:crypto"
import { setAuthentication } from "./auth.ts"
// @ts-types="./elm/main.d.ts"
import { Elm } from "./elm/main.js"

// Deno cleverly implemented `then` on `AtomicOperation` to
// throw an error that gives users a clearer error message
// when it is used to resolve a promise. We know that it is
// not a promise, and we want to be able to fulfill a promise
// with the `AtomicOperation` itself, but the presence of a
// `then` method causes the promise call that method instead
delete (Deno.AtomicOperation.prototype as any).then

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
  }
  return new Response(null, { status: 404 });
}

const app = Elm.Backend.Main.init({ flags: { globalThis } });

ConcurrentTask.register({
  tasks: {
    "property:get": ({ object, name }: { object: any, name: any }) => object[name],
    "function:apply": (args: { function: any, thisArg: any, argsArray: any }) => args.function.apply(args.thisArg, args.argsArray),
    "random:get": (bytes: number) => new Promise((resolve, reject) => randomBytes(9, (err, buf) => { if (err === null) { resolve(Array.from(buf)) } else { reject() } } )),
    "req:getBody": async (request: Request) => Array.from(new Uint8Array(await request.arrayBuffer())),
    "req:getCookie": ({ request, key }: { request: Request, key: string}) => getCookies(request.headers)[key] ?? null,
    "resp:file": ({ request, filename }: { request: Request, filename: string }) => serveFile(request, filename),
    "resp:get": ({ status, body }: { status: number, body: Array<number> }) => new Response(Uint8Array.from(body), { status }),
    "resp:legacy": handle,
  },
  ports: {
    send: app.ports.taskRequests,
    receive: app.ports.taskResponses,
  },
});

Deno.serve(async (request) => {
  return await new Promise((resolve) => {
    app.ports.requests.send({ request, resolver: resolve });
  });
});
