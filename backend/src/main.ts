import { serveFile } from "jsr:@std/http/file-server";
import { getAuthentication, setAuthentication } from "./auth.ts"

async function transact(operation) {
  while (!await operation()) {
  }
}

async function getAllWaypoints(kv) {
  const waypoints = kv.list({"prefix": ["waypoints"]});
  const result = [];
  for await (const waypoint of waypoints) {
    result.push({
      "id": waypoint.key[1],
      "text": String(waypoint.value.text),
      "completed": Boolean(waypoint.value.completed)
    });
  }
  return result;
}

Deno.serve(async (req) => {
  const route_home = new URLPattern({ pathname: "/" }).exec(req.url);
  const route_api_login = new URLPattern({ pathname: "/-/api/login" }).exec(req.url);
  const route_api_waypoints = new URLPattern({ pathname: "/-/api/waypoints" }).exec(req.url);
  const route_api_waypoints_id = new URLPattern({ pathname: "/-/api/waypoints/:id" }).exec(req.url);
  const isAuthenticated = getAuthentication(req);
  if (req.method == "GET" && route_home) {
    if (isAuthenticated) {
      return serveFile(req, "index-authenticated.html");
    } else {
      return serveFile(req, "index-unauthenticated.html");
    }
  } else if (req.method == "POST" && route_api_login) {
    const body = await req.json();
    if (typeof body.token === "string") {
      const headers = new Headers();
      setAuthentication(headers, body.token);
      return new Response(null, { headers });
    }
    return new Response(null, { status: 400 });
  }
  if (!isAuthenticated) {
    return new Response(null, { status: 403 });
  }
  if (req.method == "GET" && route_api_waypoints) {
    const kv = await Deno.openKv();
    const waypoints = await getAllWaypoints(kv);
    await kv.close()
    return new Response(JSON.stringify({waypoints: waypoints}));
  } else if (req.method == "DELETE" && route_api_waypoints_id) {
    const id = route_api_waypoints_id.pathname.groups.id;
    const kv = await Deno.openKv();
    await kv.delete(["waypoints", id]);
    await kv.close()
    return new Response("");
  } else if (req.method == "PATCH" && route_api_waypoints_id) {
    const id = route_api_waypoints_id.pathname.groups.id;
    const body = await req.json();
    const kv = await Deno.openKv();
    const key = ["waypoints", id]
    await transact(async () => {
      const entry = await kv.get(key);
      const waypoint = entry.value || {};
      if ('text' in body) {
        waypoint.text = String(body.text);
      }
      if ('completed' in body) {
        waypoint.completed = Boolean(body.completed);
      }
      return (await kv.atomic().check(entry).set(key, waypoint).commit()).ok;
    });
    await kv.close()
    return new Response("");
  }
  return new Response(null, { status: 404 });
});
