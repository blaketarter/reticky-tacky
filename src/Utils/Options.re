let windowSize = 600;
let gridSize = 3;
let spacing = 25;

let cellSize = (windowSize - spacing) / 3 - spacing;
let tokenSize = cellSize / 2;
let tokenInset = cellSize / 2 - tokenSize / 2;

let rectMode = Reprocessing_Common.Corner;

let fontColor = Reprocessing.Constants.white;

let aiEnabled = true;
let aiAutoPlay = true;
let decrementEnabled = false;
let debugAi = false;