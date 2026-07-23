import { setCookie } from "@std/http";

export function setAuthentication(headers: Headers, token: string) {
  setCookie(headers, {
    name: TOKEN_KEY_COOKIE,
    value: token,
    secure: true,
    httpOnly: true,
    sameSite: "Lax",
    maxAge: 30*24*60*60,
  });
}

const TOKEN_KEY_COOKIE = "__Host-Http-a";
