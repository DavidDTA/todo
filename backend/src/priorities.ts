import * as z from "@npm/zod"
import { transact } from "./transact.ts"

const kvEntryParser =
  z.preprocess(
    (it: any) => it === null ? {} : it,
    z.object({
      priorities: z.string().array().catch([])
    }),
  );

export async function getPriorities(kv: Deno.Kv) {
  const entry = await kv.get(["preferences"]);
  return kvEntryParser.parse(entry.value);
}

export async function updatePriorities(
  kv: Deno.Kv,
  update: {
    priorities?: string[]
  }) {
  const key = ["preferences"]
  await transact(async () => {
    const entry = await kv.get(key);
    const value = kvEntryParser.parse(entry.value);
    if (typeof update.priorities !== "undefined") {
      value.priorities = update.priorities;
    }
    return (await kv.atomic().check(entry).set(key, value).commit()).ok;
  });
}
