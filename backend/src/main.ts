import * as z from "@npm/zod";
import { serveFile } from "@std/http/file-server";
import { getAuthentication, setAuthentication } from "./auth.ts"
import { getAllWaypoints, deleteWaypoint, updateWaypoint } from "./waypoints.ts"


Deno.serve(async (req) => {
  const route_home =
    new URLPattern({ pathname: "/" }).exec(req.url);
  const route_api_login =
    new URLPattern({ pathname: "/-/api/login" }).exec(req.url);
  const route_api_waypoints =
    new URLPattern({ pathname: "/-/api/waypoints" }).exec(req.url);
  const route_api_waypoints_id =
    new URLPattern({ pathname: "/-/api/waypoints/:id" }).exec(req.url);
  const isAuthenticated = getAuthentication(req);
  if (req.method == "GET" && route_home) {
    if (isAuthenticated) {
      return serveFile(req, "index-authenticated.html");
    } else {
      return serveFile(req, "index-unauthenticated.html");
    }
  } else if (req.method == "POST" && route_api_login) {
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
  if (!isAuthenticated) {
    return new Response(null, { status: 403 });
  }
  if (req.method == "GET" && route_api_waypoints) {
    const kv = await Deno.openKv();
    const waypoints = await getAllWaypoints(kv);
    await kv.close()
    return new Response(JSON.stringify({ waypoints }));
  } else if (req.method == "DELETE" && route_api_waypoints_id) {
    const id = route_api_waypoints_id.pathname.groups.id as string;
    const kv = await Deno.openKv();
    await deleteWaypoint(kv, id);
    await kv.close()
    return new Response("");
  } else if (req.method == "PATCH" && route_api_waypoints_id) {
    const id = route_api_waypoints_id.pathname.groups.id as string;
    const body =
      z.object({
        text: z.string().optional(),
        completed: z.boolean().optional(),
        url: z.string().url().nullable().optional(),
        requires: z.string().array().optional(),
        requiredBy: z.string().array().optional()
      })
        .strict()
        .safeParse(await req.json());
    if (!body.success) {
      return new Response(null, { status: 400 });
    }
    const kv = await Deno.openKv();
    await updateWaypoint(kv, id, body.data);
    await kv.close()
    return new Response("");
  }
  return new Response(null, { status: 404 });
});
