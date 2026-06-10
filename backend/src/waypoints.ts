import * as z from "@npm/zod"

const kvEntryParser =
  z.preprocess(
    (it: any) => it === null ? {} : it,
    z.object({
      text: z.string().catch(""),
      completed: z.boolean().catch(false),
      url: z.string().url().nullable().catch(null),
      requires: z.string().array().catch([]),
      requiredBy: z.string().array().catch([]),
      source: z.string().catch("user"),
    }),
  );

export async function getAllWaypoints(kv: Deno.Kv) {
  const waypoints = kv.list({"prefix": ["waypoints"]});
  const result = [];
  for await (const entry of waypoints) {
    const waypoint = kvEntryParser.parse(entry.value);
    result.push({
      id: entry.key[1],
      text: waypoint.text,
      completed: waypoint.completed,
      url: waypoint.url,
      requires: waypoint.requires,
      requiredBy: waypoint.requiredBy,
      source: waypoint.source,
    });
  }
  return result;
}
