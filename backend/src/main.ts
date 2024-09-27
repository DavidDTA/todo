import { serveFile } from "jsr:@std/http/file-server";

Deno.serve(async (req) => {
    return serveFile(req, "build/frontend/index.html")
});
