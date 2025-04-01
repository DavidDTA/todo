import * as z from "@npm/zod"
import { transact } from "./transact.ts"

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

export async function deleteWaypoint(kv: Deno.Kv, id: string) {
  await kv.delete(["waypoints", id]);
}

export async function updateWaypoint(
  kv: Deno.Kv,
  id: string,
  update: {
    text?: string,
    completed?: boolean,
    url?: string | null,
    requires?: string[],
    requiredBy?: string[],
    source?: string,
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
    if (typeof update.url !== "undefined") {
      waypoint.url = update.url;
    }
    if (typeof update.requires !== "undefined") {
      waypoint.requires = update.requires;
    }
    if (typeof update.requiredBy !== "undefined") {
      waypoint.requiredBy = update.requiredBy
    }
    if (typeof update.source !== "undefined") {
      waypoint.source = update.source
    }
    return (await kv.atomic().check(entry).set(key, waypoint).commit()).ok;
  });
}
