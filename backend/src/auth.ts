import { setCookie } from "@std/http";

export function setAuthentication(headers: Headers, token: string) {
  setCookie(headers, {
    name: TOKEN_KEY_COOKIE,
    value: token,
    secure: true,
    httpOnly: true,
    sameSite: "Lax",
  });
}

const TOKEN_KEY_COOKIE = "__Host-d";
