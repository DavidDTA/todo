export async function transact(operation: () => Promise<boolean>) {
  while (!await operation()) {
  }
}
