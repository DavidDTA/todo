import * as z from "@npm/zod"
const kvEntryParser = z.object({
  text: z.string().catch(""),
  completed: z.boolean().catch(false)
});

async function transact(operation: () => Promise<boolean>) {
  while (!await operation()) {
  }
}

export async function getAllWaypoints(kv: Deno.Kv) {
  const waypoints = kv.list({"prefix": ["waypoints"]});
  const result = [];
  for await (const entry of waypoints) {
    const waypoint = kvEntryParser.parse(entry.value);
    result.push({
      id: entry.key[1],
      text: waypoint.text,
      completed: waypoint.completed
    });
  }
  return result;
}

export async function deleteWaypoint(kv: Deno.Kv, id: string) {
  await kv.delete(["waypoints", id]);
}

export async function updateWaypoint(
  kv: Deno.Kv,
  id: string,
  update: {
    text?: string,
    completed?: boolean
  }) {
  const key = ["waypoints", id]
  await transact(async () => {
    const entry = await kv.get(key);
    const waypoint = kvEntryParser.parse(entry.value);
    if (typeof update.text !== "undefined") {
      waypoint.text = update.text;
    }
    if (typeof update.completed !== "undefined") {
      waypoint.completed = update.completed;
    }
    return (await kv.atomic().check(entry).set(key, waypoint).commit()).ok;
  });
}
