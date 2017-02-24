// Constants

const verticalSegments1 = [false, true, false, false, true, true, false, false, false, false, false, false, true, false];
const verticalSegments2 = [false, false, false, false, true, false, true, false, false, false, false, false, true, true];
const verticalLookup = {"true,true,true,true": [[1,5],[4,5],[4,6],[4,11],[4,13],[5,5],[5,12],[8,12],[11,4],[11,6],[11,12],[11,13],[12,5],[12,12]],
"true,true,true,false": [[0,4],[0,12],[1,12],[2,12],[3,4],[3,12],[4,0],[4,1],[4,2],[4,3],[4,4],[4,7],[4,8],[4,9],[4,10],[4,12],[6,12],[7,12],[9,12],[10,12],[13,12]],
"true,true,false,true": [[0,6],[0,13],[1,11],[3,6],[3,13],[5,3],[5,11],[12,3],[12,11]],
"true,true,false,false": [[1,3]],
"true,false,true,true": [[5,4],[5,10],[7,6],[7,13],[8,4],[10,6],[10,13],[12,4],[12,10]],
"true,false,true,false": [[1,4],[1,10],[2,4],[6,4],[7,4],[9,4],[10,4],[13,4]],
"true,false,false,true": [[1,6],[1,13],[2,6],[2,13],[5,0],[5,1],[5,2],[5,6],[5,7],[5,8],[5,9],[5,13],[6,6],[6,13],[8,6],[8,13],[9,6],[9,13],[12,0],[12,1],[12,2],[12,6],[12,7],[12,8],[12,9],[12,13],[13,6],[13,13]],
"true,false,false,false": [[1,0],[1,1],[1,2],[1,7],[1,8],[1,9]],
"false,true,true,true": [[0,5],[2,5],[3,5],[6,5],[7,5],[7,11],[8,5],[9,5],[10,5],[10,11],[11,0],[11,1],[11,2],[11,3],[11,5],[11,7],[11,8],[11,9],[11,10],[11,11],[13,5]],
"false,true,true,false": [[0,10],[3,10],[7,3],[10,3]],
"false,true,false,true": [[0,11],[2,11],[3,11],[6,11],[8,3],[8,11],[9,11],[13,11]],
"false,true,false,false": [[0,0],[0,1],[0,2],[0,3],[0,7],[0,8],[0,9],[2,3],[3,0],[3,1],[3,2],[3,3],[3,7],[3,8],[3,9],[6,3],[9,3],[13,3]],
"false,false,true,true": [[8,10]],
"false,false,true,false": [[2,10],[6,10],[7,0],[7,1],[7,2],[7,7],[7,8],[7,9],[7,10],[9,10],[10,0],[10,1],[10,2],[10,7],[10,8],[10,9],[10,10],[13,10]],
"false,false,false,true": [[8,0],[8,1],[8,2],[8,7],[8,8],[8,9]],
"false,false,false,false": [[2,0],[2,1],[2,2],[2,7],[2,8],[2,9],[6,0],[6,1],[6,2],[6,7],[6,8],[6,9],[9,0],[9,1],[9,2],[9,7],[9,8],[9,9],[13,0],[13,1],[13,2],[13,7],[13,8],[13,9]]};

const horizontalSegments1 = [false, false, false, false, false, true, true, false, false, false, true, false, false, true];
const horizontalSegments2 = [false, false, true, true, false, true, false, false, false, true, false, false, false, false];
const horizontalLookup = {"true,true,true,true,true,true": [[4,9],[11,2]],
"true,true,true,true,true,false": [[5,0],[5,7],[10,1]],
"true,true,true,true,false,true": [[4,2]],
"true,true,true,true,false,false": [[4,3],[4,5]],
"true,true,true,false,true,true": [[6,1],[12,3],[13,1]],
"true,true,true,false,true,false": [[5,1],[5,3]],
"true,true,true,false,false,true": [[3,2],[8,2]],
"true,true,true,false,false,false": [[9,3]],
"true,true,false,true,true,true": [[5,10],[6,8],[13,8]],
"true,true,false,true,true,false": [[5,8],[10,8]],
"true,true,false,true,false,true": [[1,2],[10,2]],
"true,true,false,true,false,false": [[10,4]],
"true,true,false,false,true,true": [[2,2],[5,2],[5,9],[5,12],[9,9],[12,2],[12,5],[12,9]],
"true,true,false,false,true,false": [[5,4],[5,5],[5,6],[5,11],[5,13]],
"true,true,false,false,false,true": [[0,2],[6,2],[6,4],[7,2],[9,2],[13,2],[13,4]],
"true,true,false,false,false,false": [[9,5]],
"true,false,true,true,true,true": [[11,9]],
"true,false,true,true,true,false": [[11,3],[11,5]],
"true,false,true,true,false,true": [[6,0],[6,7],[13,0],[13,7]],
"true,false,true,true,false,false": [[1,3],[10,0],[10,3],[10,7]],
"true,false,true,false,true,true": [[3,9],[8,9]],
"true,false,true,false,true,false": [[2,3]],
"true,false,true,false,false,true": [[3,3],[3,5],[6,3],[13,3]],
"true,false,true,false,false,false": [[0,3],[7,3],[8,3],[8,5]],
"true,false,false,true,true,true": [[1,9],[10,9]],
"true,false,false,true,true,false": [[10,11]],
"true,false,false,true,false,true": [[6,10],[10,10],[10,12],[13,10]],
"true,false,false,true,false,false": [[1,5],[10,5],[10,6],[10,13]],
"true,false,false,false,true,true": [[0,9],[2,9],[6,9],[6,11],[7,9],[13,9],[13,11]],
"true,false,false,false,true,false": [[2,5]],
"true,false,false,false,false,true": [[6,5],[6,6],[6,12],[6,13],[13,5],[13,6],[13,12],[13,13]],
"true,false,false,false,false,false": [[0,5],[7,5]],
"false,true,true,true,true,true": [[3,8],[12,0],[12,7]],
"false,true,true,true,true,false": [[1,1],[4,1],[4,8],[4,11],[8,8],[11,1],[11,4],[11,8]],
"false,true,true,true,false,true": [[4,10],[4,12]],
"false,true,true,true,false,false": [[4,0],[4,4],[4,6],[4,7],[4,13],[9,0],[9,7]],
"false,true,true,false,true,true": [[3,1],[12,1]],
"false,true,true,false,true,false": [[0,1],[2,1],[7,1],[8,1],[9,1]],
"false,true,true,false,false,true": [[3,4]],
"false,true,true,false,false,false": [[8,4]],
"false,true,false,true,true,true": [[12,8],[12,10]],
"false,true,false,true,true,false": [[0,8],[1,8],[2,8],[7,8],[9,8]],
"false,true,false,true,false,true": [[9,10]],
"false,true,false,true,false,false": [[1,4]],
"false,true,false,false,true,true": [[12,4],[12,6],[12,11],[12,12],[12,13]],
"false,true,false,false,true,false": [[2,4],[9,11]],
"false,true,false,false,false,true": [[9,12]],
"false,true,false,false,false,false": [[0,4],[7,4],[9,4],[9,6],[9,13]],
"false,false,true,true,true,true": [[11,10],[11,12]],
"false,false,true,true,true,false": [[2,0],[2,7],[11,0],[11,6],[11,7],[11,11],[11,13]],
"false,false,true,true,false,true": [[3,0],[3,7],[3,10],[8,10]],
"false,false,true,true,false,false": [[0,0],[0,7],[1,0],[1,7],[7,0],[7,7],[8,0],[8,7]],
"false,false,true,false,true,true": [[3,11]],
"false,false,true,false,true,false": [[8,11]],
"false,false,true,false,false,true": [[3,6],[3,12],[3,13],[8,12]],
"false,false,true,false,false,false": [[8,6],[8,13]],
"false,false,false,true,true,true": [[2,10]],
"false,false,false,true,true,false": [[1,11]],
"false,false,false,true,false,true": [[0,10],[1,10],[1,12],[7,10]],
"false,false,false,true,false,false": [[1,6],[1,13]],
"false,false,false,false,true,true": [[2,12]],
"false,false,false,false,true,false": [[0,11],[2,6],[2,11],[2,13],[7,11]],
"false,false,false,false,false,true": [[0,12],[7,12]],
"false,false,false,false,false,false": [[0,6],[0,13],[7,6],[7,13]]};

const blueColour = "rgba(0, 0, 255, 0.8)";
const redColour = "rgba(255, 0, 0, 0.8)";

const startRadius = 100;
const verticalWidth = 10;
const horizontalWidth = 50;
const horizontalHeight = 0.05; // In radians
const digitGap = 10;
// const ringGap = 30;
const outsidePad = 80;

const spinSpeed = 0.001;

// const colonSeparation = 30;
// const colonSize = 5;

function totalRadius() {
  return startRadius + 4 * verticalWidth + 2 * horizontalWidth + digitGap;
}

function ringCenter() {
  const r = totalRadius() + outsidePad;
  return { x : r, y : r };
}

// function rightRingCenter() {
//   const y = totalRadius() + outsidePad;
//   const x = y + 2 * totalRadius() + ringGap;
//   return { x : x, y : y };
// }

// function middlePoint() {
//   const y = totalRadius() + outsidePad;
//   const x = y + totalRadius() + ringGap / 2;
//   return { x : x, y : y };
// }

function digitToSegments(d) {
  switch(d) {
  case 0: return [[true, true], [true, false, true], [true, true]];
  case 1: return [[false, false], [false, false, false], [true, true]];
  case 2: return [[false, true], [true, true, true], [true, false]];
  case 3: return [[false, false], [true, true, true], [true, true]];
  case 4: return [[true, false], [false, true, false], [true, true]];
  case 5: return [[true, false], [true, true, true], [false, true]];
  case 6: return [[true, true], [true, true, true], [false, true]];
  case 7: return [[false, false], [true, false, false], [true, true]];
  case 8: return [[true, true], [true, true, true], [true, true]];
  case 9: return [[true, false], [true, true, true], [true, true]];
  default: return digitToSegments(8);
  }
}

function drawFatArc(ctx, origin, startAngle, endAngle, innerRadius, outerRadius, colour) {
  ctx.beginPath();
  ctx.arc(origin.x, origin.y, outerRadius, startAngle, endAngle, false);
  ctx.arc(origin.x, origin.y, innerRadius, endAngle, startAngle, true);
  ctx.fillStyle = colour;
  ctx.fill();
}

// function drawFilledCircle(ctx, origin, radius) {
//   ctx.beginPath();
//   ctx.arc(origin.x, origin.y, radius, 0, 2 * Math.PI, false);
//   ctx.fillStyle = "black";
//   ctx.fill();
// }

function drawHorizontalSegments(ctx, segments, origin, angle, innerRadius, outerRadius, colour) {
  const segmentCount = segments.length;
  const segmentSize = 2 * Math.PI / segmentCount;

  for(var i = 0; i < segmentCount; i++) {
    if(segments[i]) {
      const baseAngle  = -angle + segmentSize * (i - 1);
      const startAngle = baseAngle - (horizontalHeight / 2);
      const endAngle   = baseAngle + (horizontalHeight / 2);
      drawFatArc(ctx, origin, startAngle, endAngle, innerRadius, outerRadius, colour);
    }
  }
}

function drawVerticalSegments(ctx, segments, origin, angle, innerRadius, outerRadius, colour) {
  const segmentCount = segments.length;
  const segmentSize = 2 * Math.PI / segmentCount;

  for(var i = 0; i < segmentCount; i++) {
    if(segments[i]) {
      const baseAngle  = -angle + segmentSize * (i - 1);
      const startAngle = baseAngle - (horizontalHeight / 2);
      const endAngle   = baseAngle + (horizontalHeight / 2) + segmentSize;
      drawFatArc(ctx, origin, startAngle, endAngle, innerRadius, outerRadius, colour);
    }
  }
}

// function drawColon(ctx) {
//   var middle = middlePoint();
//   var topOrigin = {x : middle.x, y : middle.y - colonSeparation / 2};
//   drawFilledCircle(ctx, topOrigin, colonSize);
//   var botOrigin = {x : middle.x, y : middle.y + colonSeparation / 2};
//   drawFilledCircle(ctx, botOrigin, colonSize);
// }

function drawGuide(ctx) {
  var center = ringCenter();
  var total = totalRadius();
  var segmentAngle = 2 * Math.PI / verticalSegments1.length;
  var middleHeight = total * Math.tan(segmentAngle + 0.04);
  ctx.beginPath();

  ctx.moveTo(center.x, center.y);
  ctx.lineTo(center.x - total, center.y - middleHeight);

  ctx.moveTo(center.x, center.y);
  ctx.lineTo(center.x - total, center.y + middleHeight);

  ctx.moveTo(center.x, center.y);
  ctx.lineTo(center.x + total, center.y - middleHeight);

  ctx.moveTo(center.x, center.y);
  ctx.lineTo(center.x + total, center.y + middleHeight);

  ctx.lineWidth = 2;
  ctx.strokeStyle = "#A0A0A0";
  ctx.stroke();
}

const secondStartRadius = startRadius + 2 * verticalWidth + horizontalWidth + digitGap;
var ringProps = [
  {
    innerRadius: startRadius,
    outerRadius: startRadius + verticalWidth,
    orientation: 'vertical'
  },
  {
    innerRadius: startRadius,
    outerRadius: startRadius + 2 * verticalWidth + horizontalWidth,
    orientation: 'horizontal'
  },
  {
    innerRadius: startRadius + verticalWidth + horizontalWidth,
    outerRadius: startRadius + 2 * verticalWidth + horizontalWidth,
    orientation: 'vertical'
  },
  {
    innerRadius: secondStartRadius,
    outerRadius: secondStartRadius + verticalWidth,
    orientation: 'vertical'
  },
  {
    innerRadius: secondStartRadius,
    outerRadius: secondStartRadius + 2 * verticalWidth + horizontalWidth,
    orientation: 'horizontal'
  },
  {
    innerRadius: secondStartRadius + verticalWidth + horizontalWidth,
    outerRadius: secondStartRadius + 2 * verticalWidth + horizontalWidth,
    orientation: 'vertical'
  },
];

// Fuck
var deepCopy = function( o ) {
  return JSON.parse(JSON.stringify( o ));
};

function drawRing(ctx, ring, vsegments, hsegments, colour) {
  const origin = ringCenter();
  for(let i in ring) {
    let r = ring[i];
    if(r.orientation === 'vertical') {
      drawVerticalSegments(ctx, vsegments, origin, r.angle, r.innerRadius, r.outerRadius, colour);
    } else {
      drawHorizontalSegments(ctx, hsegments, origin, r.angle, r.innerRadius, r.outerRadius, colour);
    }
  }
}

function goalDigits() {
  var currentDate = new Date();
  var hours = currentDate.getHours();
  var minutes = currentDate.getMinutes();
  return [Math.floor(hours / 10), hours % 10, Math.floor(minutes / 10), minutes % 10];
}

function goalSegments() {
  let g = goalDigits();
  let r = [g[2], g[3], g[0], g[1]];
  var t = r.map(digitToSegments);
  return [].concat.apply([], t);
}

// function findIndex(arr, target, current) {
//   let i = current;
//   while(true) {
//     let match = true;
//     for(let j = 0; j < target.length; j++) {
//       if(arr[i + j] != target[j])
//         match = false;
//     }
//     if(match)
//       return i;

//     i += 1;
//     if(i >= arr.length) {
//       i = 0;
//     }
//     // if(i == current) {
//     //   console.log("Couldn't find", target, "in", arr, "starting at", current);
//     // }
//   }
// }

function pickRandom(array) {
  return array[Math.floor(Math.random() * array.length)];
}

function setGoalIndices(blueRing, redRing) {
  var goals = goalSegments();
  for(let i in [0,1,2,3,4,5]) {
    let b = blueRing[i];
    let r = redRing[i];
    if(b.orientation === 'vertical') {
      let key = goals[i].concat(goals[11-i]);
      let goalIndices = pickRandom(verticalLookup[key]);
      b.goalIndex = goalIndices[0];
      r.goalIndex = goalIndices[1];
      var segmentCount = verticalSegments1.length;
    } else {
      let key = goals[i].concat(goals[11-i]);
      let goalIndices = pickRandom(horizontalLookup[key]);
      b.goalIndex = goalIndices[0];
      r.goalIndex = goalIndices[1];
      var segmentCount = horizontalSegments1.length;
    }
    b.previousIndex = b.goalIndex;
    r.previousIndex = r.goalIndex;

    const segmentSize = 2 * Math.PI / segmentCount;
    b.goalAngle = segmentSize * b.goalIndex;
    r.goalAngle = segmentSize * r.goalIndex;
  }
}

function run() {
  var canvas = document.getElementById('canvas');
  var ctx = canvas.getContext('2d');

  canvas.width  = window.innerWidth;
  canvas.height = window.innerHeight;

  var blueRing = deepCopy(ringProps);
  for(let i in blueRing) {
    blueRing[i].angle = 0;
    blueRing[i].previousIndex = 0;
  }

  var redRing = deepCopy(ringProps);
  for(let i in redRing) {
    redRing[i].angle = 0;
    redRing[i].previousIndex = 0;
  }

  var lastTime = [];
  function checkTime() {
    if(goalDigits().toString() != lastTime.toString()) { // FUCK
      setGoalIndices(blueRing, redRing);
    }
    lastTime = goalDigits();
  }
  checkTime();
  window.setInterval(checkTime, 1000);


  var lastTimestamp = undefined;
  function draw(timestamp) {
    if(lastTimestamp === undefined) {
      lastTimestamp = timestamp;
      window.requestAnimationFrame(draw);
      return;
    }

    var delta = Math.min(timestamp - lastTimestamp, 100);
    lastTimestamp = timestamp;

    for(let i in blueRing) {
      blueRing[i].angle += (blueRing[i].goalAngle - blueRing[i].angle) * delta * spinSpeed;
    }
    for(let i in redRing) {
      redRing[i].angle += (redRing[i].goalAngle - redRing[i].angle) * delta * spinSpeed;
    }

    ctx.clearRect(0, 0, canvas.width, canvas.height);
    drawRing(ctx, blueRing, verticalSegments1, horizontalSegments1, blueColour);
    drawRing(ctx, redRing,  verticalSegments2, horizontalSegments2, redColour);
    // if(Math.floor(timestamp / 1000) % 2 == 0)
    //   drawColon(ctx);
    drawGuide(ctx);

    window.requestAnimationFrame(draw);
  }

  window.requestAnimationFrame(draw);
}

run();
