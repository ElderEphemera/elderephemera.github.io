const tableNode = document.getElementById("table");

function makeImageCard(name) {
  let img = document.createElement("img");
  img.src = `images/${name}.svg`;
  return { name, contents: img };
}

const fruit = [
  "apple",
  "banana",
  "pear",
  "orange",
  "grapes",
].map(makeImageCard);

const skull = makeImageCard("skull");
const bonus = makeImageCard("bonus");

const specials = {
  skull: -50,
  bonus: 100,
};

function rand(n) {
  return Math.floor(Math.random()*n);
}

function shuffle(arr) {
  return arr
    .map(el => ({ el, key: Math.random() }))
    .sort((x, y) => x.key - y.key)
    .map(tagged => tagged.el);
}

function chunk(n, arr) {
  const chunked = [];
  for(; arr.length > 0; arr = arr.slice(n)) {
    chunked.push(arr.slice(0, n));
  }
  return chunked;
}

function twice(elem) {
  return [0,0].map(() => elem());
}

function thrice(elem) {
  return [0,0,0].map(() => elem());
}

function makeTable(parent, spread, cls) {
  for (const row of spread) {
    const rowNode = document.createElement("tr");
    for (const card of row) {
      const placeNode = document.createElement("td");
      const cardNode = document.createElement("div");
      cardNode.classList.add(card.classOverride || cls);
      if (specials[card.name]) cardNode.classList.add("special");
      cardNode.setAttribute("card-name", card.name);
      cardNode.appendChild(card.contents.cloneNode(true));
      placeNode.appendChild(cardNode);
      rowNode.appendChild(placeNode);
    }
    parent.appendChild(rowNode);
  }
}

function composite(name, spread) {
  const compTable = document.createElement("table");
  compTable.classList.add("composite");
  makeTable(compTable, spread, "comp-ico");
  return { name, contents: compTable };
}

function mapSpread(spread, func) {
  return spread.map(row => row.map(func));
}

function signature(spread) {
  const names = spread.flat().map(card => card.name);
  return spread.map(row => row.map(card => names.indexOf(card.name)).join("")).join();
}

function insertRow(spread, rowIndex, newRow) {
  return spread.toSpliced(rowIndex, 0, newRow);
}

function insertCol(spread, colIndex, newCol) {
  return spread.map((row, i) => row.toSpliced(colIndex, 0, newCol[i]));
}

function genNormal(choices, n, w) {
  let chosen = shuffle(choices).slice(0, 3);
  return chunk(3, shuffle(chosen.concat(chosen)));
}

function genText(headerText, text) {
  const header = document.createElement("h1");
  header.textContent = headerText
  const card = document.createElement("h2");
  card.textContent = text;
  return [
    [{ name: "header", contents: header, classOverride: "header" }],
    twice(() => ({ name: text, contents: card })),
  ]
}

function large(basis) {
  return insertCol(insertRow(basis, extraRow, largeExtraRow), extraCol, largeExtraCol);
}

function minefield(mine, fruit) {
  const field = thrice(() => thrice(() => mine));
  field[minefieldLoc1.y][minefieldLoc1.x] = fruit;
  field[minefieldLoc2.y][minefieldLoc2.x] = fruit;
  return field;
}

function small(fruit) {
  return chunk(2, shuffle([skull, bonus, ...twice(() => fruit)]));
}

let currentLevel = 0;

let mainFruit;
let extraFruit1, extraFruit2;
let levelA, levelB, levelC;
let firstRun = true;
let extraRow, extraCol, specialIx;
let minefieldLoc1 = {}, minefieldLoc2 = {};
let largeExtraRow, largeExtraCol;

function initializeLevels() {
  score = 0;
  count(document.getElementById("counter"), "--score", 0, 0);

  mainFruit = shuffle(fruit);
  extraFruit1 = mainFruit.pop();
  extraFruit2 = mainFruit.pop();

  do {
    levelA = genNormal(mainFruit, 3, 3);
  } while (signature(levelA) == signature(levelA.toReversed()))
  do {
    levelB = genNormal(mainFruit, 3, 3);
  } while (signature(levelA) == signature(levelB))
  do {
    levelC = genNormal(mainFruit, 3, 3);
  } while ([signature(levelA), signature(levelB)].includes(signature(levelC)))

  extraRow = firstRun ? 1 : rand(3);
  extraCol = firstRun ? 2 : rand(4);
  specialIx = firstRun ? 1 : rand(3);
  firstRun = false;

  minefieldLoc1.x = rand(3);
  minefieldLoc1.y = rand(3);
  minefieldLoc2.x = rand(3);
  do {
    minefieldLoc2.y = rand(3);
  } while (minefieldLoc1.x == minefieldLoc2.x && minefieldLoc1.y == minefieldLoc2.y)

  let additional = shuffle([...twice(() => extraFruit1), ...twice(() => extraFruit2), skull, bonus]);
  largeExtraRow = additional.slice(0,3);
  largeExtraCol = additional.slice(3,6);
}

let clickHook = undefined;

function clickFirst(name) {
  clickHook = clickedCard => {
    let firstCard = document.querySelector(`[card-name=${name}]`);
    let firstCardParent = firstCard.parentElement;
    clickedCard.replaceWith(firstCard);
    firstCardParent.appendChild(clickedCard);
    clickHook = undefined;
    return firstCard;
  };
}

const levels = [
  // Ordinary world
  () => {
    initializeLevels();
    return levelA;
  },
  () => levelA,
  () => levelA,

  // Call to adventure
  () => {
    clickFirst("levelA");
    let types =
      Object.entries({ levelA, levelB, levelC }).map(e => composite(...e));
    return chunk(3, shuffle(types.concat(types)));
  },

  // Refusal of the call
  () => levelB,
  () => levelC,
  () => levelA,

  // Meeting with the mentor
  () => genText("Nice Job!", "Think Carefully"),

  // Crossing the first threshold
  () => mapSpread(levelA.toReversed(), ({ name, contents }) => {
    contents = contents.cloneNode(true);
    contents.style.transform = "rotateX(180deg)";
    return { name, contents };
  }),
  () => mapSpread(levelA.map(row => row.toReversed()), ({ name, contents}) => {
    contents = contents.cloneNode(true);
    contents.style.transform = "rotateY(180deg)";
    return { name, contents };
  }),

  // Tests, allies, and enemies
  () => insertCol(levelA, extraCol, twice(() => extraFruit1)),
  () => insertRow(levelA, extraRow, insertRow(twice(() => extraFruit1), specialIx, bonus)),
  () => insertRow(levelA, extraRow, insertRow(twice(() => extraFruit1), specialIx, skull)),

  // The innermost cave
  () => large(levelA),
  () => large(levelB),
  () => large(levelC),

  // The ordeal
  () => minefield(skull, extraFruit1),
  () => genText("Sorry", "Trust Me"),

  // Reward
  () => minefield(bonus, extraFruit1),

  // The road back
  () => levelB,
  () => levelC,

  // The resurrection
  () => small(extraFruit2),

  // Return with the elixir
  () => levelA,
  () => genText("You Win!", "Play Again"),
];

let selected = null;
let playing = false;

async function nextLevel() {
  tableNode.textContent = "";
  makeTable(tableNode, levels[currentLevel++ % levels.length](), "card");
  await changeCards(true);
  playing = true;
}

function slide(nodes, enter, delay) {
  const keyframes = enter
    ? [{ translate: `0 1000px` }, {}]
    : [{}, { translate: `0 -1000px` }];
  const timing = { duration: 300, easing: "ease", fill: "both" };
  return Promise.all(Array.from(nodes).map(
    (node, i) => node.animate(keyframes, { delay: i*delay, ...timing }).finished
  ));
}

function changeCards(enter) {
  return slide(document.querySelectorAll(".card, .header"), enter, 100);
}

function rotate(card, from, to, direction) {
  return card.animate(
    [{ transform: `rotateY(${from}deg)` }, { transform: `rotateY(${to}deg)` }],
    { duration: 100, easing: `ease-${direction}`, composite: "add" }
  ).finished;
}

async function flip(card) {
  await rotate(card, 180, 90, "in");
  card.classList.add("flipped");
  await rotate(card, 90, 0, "out");
}

async function unflip() {
  let flipped =
    Array.from(document.getElementsByClassName("flipped"))
      .filter(card => !card.classList.contains("matched"));
  await Promise.all(flipped.map(card => rotate(card, 0, 90, "in")));
  flipped.map(card => card.classList.remove("flipped"));
  await Promise.all(flipped.map(card => rotate(card, 90, 180, "out")));
  playing = true;
}

function checkFinished() {
  if (document.querySelectorAll(".card:not(.matched):not(.special)").length == 0) {
    playing = false;
    unflippedSpecials =
      Array.from(document.querySelectorAll(".special:not(.flipped)"));
    setTimeout(async () => {
      await unflippedSpecials.reduce(
        (promise, card) => promise.then(() => flip(card)),
        Promise.resolve(0));
      await changeCards(false);
      nextLevel();
    }, 1000);
  }
}

function count(node, counter, from, to) {
  return node.animate(
    [{ [counter]: from }, { [counter]: to }],
    {
      duration: Math.sqrt(Math.abs(to-from))*150,
      delay: 500,
      fill: "both"
    }
  ).finished;
}

let score = 0;
let streak = 0;
let adding = Promise.resolve();

function addPoints(n) {
  adding = adding.then(() => {
    const counter = document.getElementById("counter");
    const addend = document.getElementById("addend");
    if (n >= 0) {
      addend.classList.add("positive");
      addend.classList.remove("negative");
    } else {
      addend.classList.add("negative");
      addend.classList.remove("positive");
    }
    addend.style.visibility = "visible";
    return Promise.all([
      count(counter, "--score", score, score+n),
      count(addend, "--addend", n, 0),
    ]).then(() => {
      score += n;
      addend.style.visibility = "hidden";
    });
  });
}

tableNode.onclick = event => {
  let target = event.target;

  if (!playing) return;
  if (!target.classList.contains("card")) return;
  if (target == selected) return;

  if (clickHook) target = clickHook(target) || target;

  flip(target);
  const cardName = target.getAttribute("card-name");

  if (specials[cardName]) {
    addPoints(specials[cardName]);
    target.classList.add("matched");
    if (specials[cardName] < 0) streak = 0;
  } else if (selected == null) {
    unflip();
    selected = target;
  } else {
    if (cardName == selected.getAttribute("card-name")) {
      target.classList.add("matched");
      selected.classList.add("matched");
      if (cardName != "Play Again") {
        addPoints(10+5*streak);
        streak++;
      }
      checkFinished();
    } else {
      addPoints(-5);
      streak = 0;
      setTimeout(unflip, 1000);
      playing = false;
    }
    selected = null;
  }
};

nextLevel();
