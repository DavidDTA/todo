
async function transact(operation: () => Promise<boolean>) {
  while (!await operation()) {
  }
}

export async function getAllWaypoints(kv: Deno.Kv) {
  const waypoints = kv.list<{
    id: string,
    text: string,
    completed: boolean
  }>({"prefix": ["waypoints"]});
  const result = [];
  for await (const waypoint of waypoints) {
    result.push({
      id: waypoint.key[1],
      text: String(waypoint.value.text),
      completed: Boolean(waypoint.value.completed)
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
    const waypoint: {
      text?: string,
      completed?: boolean
    } = entry.value || {};
    if (typeof update.text !== "undefined") {
      waypoint.text = update.text;
    }
    if (typeof update.completed !== "undefined") {
      waypoint.completed = update.completed;
    }
    return (await kv.atomic().check(entry).set(key, waypoint).commit()).ok;
  });
}
