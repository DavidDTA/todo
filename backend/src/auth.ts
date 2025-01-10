import { getCookies, setCookie } from "@std/http";

export function getAuthentication(req: Request) {
  return Deno.env.get(TOKEN_KEY_ENV) === getCookies(req.headers)[TOKEN_KEY_COOKIE];
}

export function setAuthentication(headers: Headers, token: string) {
  setCookie(headers, {
    name: TOKEN_KEY_COOKIE,
    value: token,
    secure: true,
    httpOnly: true,
    sameSite: "Lax",
  });
}

const TOKEN_KEY_ENV = "TOKEN";
const TOKEN_KEY_COOKIE = "__Host-d";
