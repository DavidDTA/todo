import { serveFile } from "jsr:@std/http/file-server";

function transact(operation) {
  while (!operation()) {
  }
}

async function getAllWaypoints(kv) {
  const waypoints = kv.list({"prefix": ["waypoints"]});
  const result = [];
  for await (waypoint of waypoints) {
    result.push({
      "id": waypoint.key[1],
      "text": String(waypoint.value.text),
      "complete": Boolean(waypoint.value.complete)
    });
  }
  return result;
}

Deno.serve(async (req) => {
  const route_home = new URLPattern({ pathname: "/" }).exec(req.url);
  const route_api_waypoints = new URLPattern({ pathname: "/-/api/waypoints" }).exec(req.url);
  const route_api_waypoints_id = new URLPattern({ pathname: "/-/api/waypoints/:id" }).exec(req.url);
  if (req.method == "GET" && route_home) {
    return serveFile(req, "build/frontend/index.html");
  } else if (req.method == "GET" && route_api_waypoints) {
    const kv = await Deno.openKv();
    const waypoints = await getAllWaypoints(kv);
    kv.close()
    return new Response(JSON.stringify({"waypoints": waypoints}));
  } else if (req.method == "DELETE" && route_api_waypoints_id) {
    const id = route_api_waypoints_id.pathname.groups.id;
    const kv = await Deno.openKv();
    kv.close()
    await kv.delete(["waypoints", id]);
    return new Response("");
  } else if (req.method == "PATCH" && route_api_waypoints_id) {
    const id = route_api_waypoints_id.pathname.groups.id;
    const body = await req.json();
    const kv = await Deno.openKv();
    const key = ["waypoints", id]
    transact(async () => {
      const entry = await kv.get(key);
      const waypoint = entry.value || {};
      if ('text' in body) {
        waypoint.text = String(body.text);
      }
      if ('completed' in body) {
        waypoint.completed = Boolean(body.complete);
      }
      return kv.atomic().check(entry).set(key, waypoint).commit().ok;
    });
    kv.close()
    return new Response("");
  }
  return new Response(null, { "status": 404 });
});
