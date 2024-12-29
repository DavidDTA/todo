import { getCookies, setCookie } from "jsr:@std/http";

export function getAuthentication(req) {
  return Deno.env.get(TOKEN_KEY_ENV) === getCookies(req.headers)[TOKEN_KEY_COOKIE];
}

export function setAuthentication(headers, token) {
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
