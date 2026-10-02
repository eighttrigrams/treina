// The check suite for copying rendered markdown (`et.trn.ui.md-copy`): a selection
// copied out of rendered markdown puts its markdown on the clipboard, as text/plain
// only. One async arrow function, evaluated in the page of a *dev* build (it calls
// the namespace by its global name, which a release build does not keep).
//
// The run: make a training whose description is `md-copy-fixture.md` (named ZZ…,
// so it is plainly a throwaway), open the Trainings page, reload, and evaluate
// this file's contents. It expands that training's card itself. It returns
// {passed, of, failed, results}.
//
//     jq -n --arg d "$(cat test/browser/md-copy-fixture.md)" \
//       '{name:"ZZ copy check", description:$d}' \
//       | curl -s -X POST -H 'Content-Type: application/json' -d @- \
//           http://127.0.0.1:3150/api/trainings/
//
// **Why the rules are checked here and not in a node suite.** Treina has no
// ClojureScript test build, and adding one means editing shadow-cljs.edn and the
// Makefile, every line of which the owner wrote. The namespace is tracker's
// `et.tr.ui.md-copy` with the names changed (treina renders markdown as tracker
// does), and tracker's `md_copy_test.cljs` holds the rules under node; part one
// below runs the same cases against treina's own copy, in the page, so a drift
// between the two would show here.
//
// Part two is the DOM half: the wrappers, the boundary points, the container.
// Option+C wants a real key press (`navigator.clipboard`); drive it from the
// session as tracker's feature does: select, press Alt+KeyC, read the clipboard.
//
// Part three is the property (the review's B2): 400 seeded random selections over
// a second training made from `md-copy-corpus.md`, named ZZ copy corpus, the same
// way as the first. None may copy wrong text; at most 40 may decline (raw-HTML cuts).
// Mutations run: entity decoding off reddened it through 77 declines (the guard at
// work), and with the guard off as well it found 52 wrong copies.
async () => {
  const wait = ms => new Promise(r => setTimeout(r, ms));
  const until = async (fn, ms = 8000) => {
    const t0 = Date.now();
    while (Date.now() - t0 < ms) { const v = fn(); if (v) return v; await wait(50); }
    return null;
  };
  const R = [];
  const check = async (name, fn) => {
    try { const {pass, evidence} = await fn(); R.push({name, pass: !!pass, evidence}); }
    catch (e) { R.push({name, pass: false, evidence: {threw: String((e && e.stack) || e)}}); }
  };
  const mc = window.et.trn.ui.md_copy;

  // --- part one: the rules, as tracker's node suite states them ----------------------
  const between = (src, from, to, nth = 0) => {
    let a = src.indexOf(from);
    for (let k = nth; k > 0; k--) a = src.indexOf(from, a + 1);
    const b = src.indexOf(to, a) + to.length;
    return mc.between(mc.analyse(src, false), a, b);
  };
  const RULES = [
    ['a cut bold run keeps its markers', 'This is **very important** stuff.', 'important', 'important', 0, '**important**'],
    ['the author\'s spelling of them', 'This is __very important__ stuff.', 'important', 'important', 0, '__important__'],
    ['a cut link keeps its URL', 'See [the docs](https://x.com/a "T") now', 'docs', 'docs', 0, '[docs](https://x.com/a "T")'],
    ['nested constructs, outermost first', 'a **bold [link](u) x** b', 'li', 'li', 0, '**[li](u)**'],
    ['a code span', 'run `make test` now', 'test', 'test', 0, '`test`'],
    ['a word in a heading, no markup', '## Heading here\n\npara', 'Heading', 'Heading', 0, 'Heading'],
    ['a whole heading, with its markup', '## Heading here\n\npara', 'Heading', 'here', 0, '## Heading here'],
    ['a whole list item', '- item one\n- item two', 'item', 'one', 0, '- item one'],
    ['list items give their lines', '- item one\n- item __two__\n  continued\n\nafter', 'one', 'two', 0, '- one\n- item __two__'],
    ['a task item keeps its box', '- [ ] task one\n- [x] done', 'task', 'done', 0, '- [ ] task one\n- [x] done'],
    ['a quote, across blocks', '> quote line\n> more\n\npara', 'line', 'para', 0, '> line\n> more\n\npara'],
    ['code inside comes plain', '```js\nlet a = 1;\nlet b = 2;\n```', 'a = 1', 'let b', 0, 'a = 1;\nlet b'],
    ['a fence cut at an edge', 'para\n\n```js\nlet a = 1;\n```', 'para', 'let a', 0, 'para\n\n```js\nlet a\n```'],
    ['a table across rows', '| a | b |\n|---|---|\n| 1 | **2** |\n| 3 | 4 |', '3', '4', 0, '| a | b |\n|---|---|\n| 3 | 4 |'],
    ['a nested item, dedented', '- a\n  - b\n    more\n- c', 'b', 'more', 0, '- b\n  more'],
    // the review's fixes (tracker's md_copy_test has them through md-point too)
    ['S5 whitespace outside the markers', 'Please **does something** now', 'does ', 'does ', 0, '**does** '],
    ['S2 raw HTML cut inside: declined', '<div>\nhello <b>x</b>\n</div>\n\npara', 'llo', 'llo', 0, null],
    ['S2 raw HTML, all its words: whole', '<div>\nhello <b>x</b>\n</div>\n\npara', 'hello', 'x', 0, '<div>\nhello <b>x</b>\n</div>'],
    ['N1 a tab-indented first item loses it, siblings stay siblings', '- nested\n\t- child\n\t- another\n- lazy', 'child', 'lazy', 0, '- child\n- another\n- lazy'],
    ['N1 code keeps its indentation', 'para\n\n    code\n    more', 'code', 'more', 0, '    code\n    more'],
    ['S1 an entity is atomic to an edge', 'a &lt;b&gt; tag', 'lt', 'gt', 0, '&lt;b&gt;'],
  ];
  for (const [name, src, from, to, nth, expected] of RULES) {
    await check('rule: ' + name, () => {
      const got = between(src, from, to, nth);
      return {pass: got === expected || (expected === null && got == null), evidence: {got, expected}};
    });
  }

  // --- part two: the page -------------------------------------------------------------
  const card = () => [...document.querySelectorAll('.card')].find(c => c.textContent.includes('ZZ copy check'));
  if (!card()) throw new Error('no training named ZZ copy check: see the header for how to make one');
  if (!card().querySelector('[data-md-src]')) {
    card().querySelector('.card-header').click();
    await until(() => card().querySelector('[data-md-src]'));
  }
  const body = () => card().querySelector('[data-md-src]');
  const selectIn = (root, from, to) => {
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const nodes = []; let all = '';
    while (walker.nextNode()) { nodes.push([walker.currentNode, all.length]); all += walker.currentNode.nodeValue; }
    const a = all.indexOf(from);
    const b = all.indexOf(to, a) + to.length;
    if (a < 0 || b < to.length) throw new Error(`"${from}"…"${to}" not in ${JSON.stringify(all.slice(0, 200))}`);
    const at = (i, end) => {
      for (const [n, o] of nodes) if (end ? i <= o + n.length : i < o + n.length) return [n, i - o];
      const [n] = nodes[nodes.length - 1]; return [n, n.length];
    };
    const r = document.createRange();
    r.setStart(...at(a, false)); r.setEnd(...at(b, true));
    return r;
  };
  const copy = (range) => {
    const sel = getSelection(); sel.removeAllRanges(); sel.addRange(range);
    const dt = new DataTransfer();
    const ev = new ClipboardEvent('copy', {clipboardData: dt, bubbles: true, cancelable: true});
    document.dispatchEvent(ev);
    sel.removeAllRanges();
    return {taken: ev.defaultPrevented, plain: dt.getData('text/plain'), html: dt.getData('text/html')};
  };
  const expectCopy = (range, expected) => {
    const got = copy(range);
    return {pass: got.taken && got.plain === expected && got.html === '', evidence: {got, expected}};
  };
  const whole = (el) => { const r = document.createRange(); r.selectNodeContents(el); return r; };

  await check('page: one wrapper per block, the container carrying its text', () => {
    const kinds = [...body().querySelectorAll(':scope > .markdown-block[data-start]')]
      .map(w => w.firstElementChild?.tagName.toLowerCase());
    return {pass: kinds.join() === 'h1,p,ul,blockquote,pre,table'
                  && body().getAttribute('data-md-src').startsWith('# Copy check heading'),
            evidence: {kinds}};
  });
  await check('page: a whole description copies verbatim, text/plain only', () =>
    expectCopy(whole(body()), body().getAttribute('data-md-src')));
  await check('page: inside a link inside bold', () => {
    const a = body().querySelector('strong a').firstChild;
    const r = document.createRange(); r.setStart(a, 1); r.setEnd(a, 3);
    return expectCopy(r, '**[in](https://example.com/x)**');
  });
  await check('page: the full stop after a link stays outside its URL', () =>
    expectCopy(selectIn(body(), 'See', 'docs.'), 'See [docs](https://x.com).'));
  await check('page: list items give their lines', () =>
    expectCopy(selectIn(body(), 'one', 'two'), '- one\n- item __two__'));
  await check('page: code inside a fence comes plain', () =>
    expectCopy(selectIn(body(), 'a = 1', 'a = 1'), 'a = 1'));
  await check('page: a triple-click shape (the end at the next block) is the paragraph', () => {
    const p = body().querySelector(':scope > .markdown-block > p');
    const r = document.createRange();
    r.setStart(p.firstChild, 0);
    r.setEnd(p.parentElement.nextElementSibling, 0);
    return expectCopy(r, 'Para with **bold [link](https://example.com/x)** and `code`. See [docs](https://x.com).\nsecond line');
  });
  await check('page: a selection into the card around it is left to the browser', () => {
    const r = selectIn(body(), 'Copy', 'Copy');
    r.setEnd(card(), card().childNodes.length);
    return {pass: !copy(r).taken, evidence: {}};
  });

  // --- part three: the property (review B2) ------------------------------------------
  // A second training whose description is `md-copy-corpus.md` (entities, [X]/[x],
  // tabs, raw HTML, nested and lazy lists, emphasis with inner spaces, a table, a
  // fence), named ZZ copy corpus. 400 random selections with a fixed seed: none
  // may copy wrong text, and at most 40 may decline (the raw-HTML cuts, by design).
  await check('property: random selections copy what was selected, or decline', async () => {
    const corpus = [...document.querySelectorAll('.card')].find(c => c.textContent.includes('ZZ copy corpus'));
    if (!corpus) throw new Error('no training named ZZ copy corpus: see the header');
    if (!corpus.querySelector('[data-md-src]')) {
      corpus.querySelector('.card-header').click();
      await until(() => corpus.querySelector('[data-md-src]'));
    }
    const el = corpus.querySelector('[data-md-src]');
    let s = 4354 >>> 0;
    const rand = () => { s = (s + 0x6D2B79F5) >>> 0; let t = s; t = Math.imul(t ^ (t >>> 15), t | 1);
                         t ^= t + Math.imul(t ^ (t >>> 7), t | 61); return ((t ^ (t >>> 14)) >>> 0) / 4294967296; };
    const w = document.createTreeWalker(el, NodeFilter.SHOW_TEXT); const nodes = []; let total = 0;
    while (w.nextNode()) { nodes.push([w.currentNode, total]); total += w.currentNode.nodeValue.length; }
    const at = (i) => { for (const [n, o] of nodes) if (i <= o + n.length) return [n, i - o];
                        const [n] = nodes[nodes.length - 1]; return [n, n.length]; };
    const sq = (x) => (x || '').replace(/\s+/g, '');
    const out = {exact: 0, declined: 0, wrong: 0, samples: []};
    for (let k = 0; k < 400; k++) {
      let a = Math.floor(rand() * total), b = Math.floor(rand() * total);
      if (a > b) [a, b] = [b, a];
      if (b === a) b = Math.min(total, a + 1 + Math.floor(rand() * 20));
      const r = document.createRange(); r.setStart(...at(a)); r.setEnd(...at(b));
      const want = sq(r.toString());
      if (!want) continue;
      const got = copy(r);
      if (!got.taken) { out.declined++; continue; }
      const shown = sq(window.et.trn.ui.markdown.rendered_text(got.plain, false));
      const table = /^\s*\|?\s*:?-{3,}/m.test(got.plain);
      if (got.html === '' && (shown === want || (table && shown.includes(want)) || sq(got.plain) === want)) out.exact++;
      else { out.wrong++; if (out.samples.length < 5) out.samples.push({selected: r.toString(), md: got.plain}); }
    }
    return {pass: out.wrong === 0 && out.exact > 200 && out.declined <= 40, evidence: out};
  });

  const failed = R.filter(r => !r.pass);
  return {passed: R.length - failed.length, of: R.length, failed: failed.map(r => r.name), results: R};
}
