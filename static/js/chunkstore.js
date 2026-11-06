// Keeps recording chunks in IndexedDB until the server has them, so a
// crashed or closed tab, a reload or a long network outage loses nothing.
// The recorder page offers to send what is left on the next visit.
//
// Stores (database "aufnahme", version 1):
//   takes:  {base, projectUrl, starts: {screen, cam}, created}  key: base
//           (base is the take's API URL, ".../takes/<tid>")
//   chunks: {base, track, seq, blob}                             key: auto
// Every call swallows storage errors (private mode, quota): the recording
// then simply works without the safety net.

let dbPromise = null;

function db() {
  if (!dbPromise) {
    dbPromise = new Promise((resolve, reject) => {
      const req = indexedDB.open("aufnahme", 1);
      req.onupgradeneeded = () => {
        const d = req.result;
        d.createObjectStore("takes", { keyPath: "base" });
        const chunks = d.createObjectStore("chunks", { autoIncrement: true });
        chunks.createIndex("base", "base");
      };
      req.onsuccess = () => resolve(req.result);
      req.onerror = () => reject(req.error);
    }).catch(() => null);
  }
  return dbPromise;
}

function run(storeNames, mode, fn) {
  return db().then(
    (d) =>
      d &&
      new Promise((resolve) => {
        let result;
        const tx = d.transaction(storeNames, mode);
        Promise.resolve(fn(tx)).then((r) => (result = r));
        tx.oncomplete = () => resolve(result);
        tx.onerror = tx.onabort = () => resolve(null);
      })
  ).catch(() => null);
}

const request = (r) => new Promise((resolve) => {
  r.onsuccess = () => resolve(r.result);
  r.onerror = () => resolve(null);
});

export function saveTake(take) {
  return run(["takes"], "readwrite", (tx) => request(tx.objectStore("takes").put(take)));
}

// Resolves to the chunk's key (to delete it once sent), or null.
export function saveChunk(base, track, seq, blob) {
  return run(["chunks"], "readwrite", (tx) =>
    request(tx.objectStore("chunks").add({ base, track, seq, blob })));
}

export function deleteChunk(key) {
  if (key == null) return Promise.resolve();
  return run(["chunks"], "readwrite", (tx) => request(tx.objectStore("chunks").delete(key)));
}

// Forgets a take and any chunks still stored for it.
export function forgetTake(base) {
  return run(["takes", "chunks"], "readwrite", async (tx) => {
    tx.objectStore("takes").delete(base);
    const keys = await request(tx.objectStore("chunks").index("base").getAllKeys(base));
    for (const k of keys || []) tx.objectStore("chunks").delete(k);
  });
}

// [{take, chunks: [{key, track, seq, blob}]}] for every stored take.
export async function leftovers() {
  const result = await run(["takes", "chunks"], "readonly", async (tx) => {
    const takes = (await request(tx.objectStore("takes").getAll())) || [];
    const out = [];
    for (const take of takes) {
      const index = tx.objectStore("chunks").index("base");
      const keys = (await request(index.getAllKeys(take.base))) || [];
      const values = (await request(index.getAll(take.base))) || [];
      out.push({ take, chunks: values.map((v, i) => ({ key: keys[i], ...v })) });
    }
    return out;
  });
  return result || [];
}
