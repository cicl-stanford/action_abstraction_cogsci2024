function UUID() {
  var baseName = (Math.floor(Math.random() * 10) + '' +
    Math.floor(Math.random() * 10) + '' +
    Math.floor(Math.random() * 10) + '' +
    Math.floor(Math.random() * 10));
  var template = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx';
  var id = baseName + '-' + template.replace(/[xy]/g, function(c) {
    var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
    return v.toString(16);
  });
  return id;
};
  
function convertToHSL(color) {
  return `hsl(${color.h}, ${color.s}%, ${color.l}%)`;
}

// draws a rounded rectangle on the canvas
function drawRoundedRect(ctx, x, y, width, height, radius, fillStyle) {
  ctx.beginPath();
  ctx.moveTo(x + radius, y);
  ctx.lineTo(x + width - radius, y);
  ctx.quadraticCurveTo(x + width, y, x + width, y + radius);
  ctx.lineTo(x + width, y + height - radius);
  ctx.quadraticCurveTo(x + width, y + height, x + width - radius, y + height);
  ctx.lineTo(x + radius, y + height);
  ctx.quadraticCurveTo(x, y + height, x, y + height - radius);
  ctx.lineTo(x, y + radius);
  ctx.quadraticCurveTo(x, y, x + radius, y);
  ctx.closePath();

  // Set the fill style if provided and fill the rectangle
  if (fillStyle) {
    ctx.fillStyle = fillStyle;
    ctx.fill();
  }
}

// defined parametrization for hue in HSL coordinates
function interpolateHue(baseHue, pathHue, t) {
  let distance;
  if (Math.abs(pathHue - baseHue) <= 180) {
      // The direct path is the shortest
      distance = pathHue - baseHue;
  } else {
      // We need to go around the hue circle
      if (pathHue > baseHue) {
          distance = pathHue - baseHue - 360;
      } else {
          distance = pathHue - baseHue + 360;
      }
  }
  let interpolatedHue = baseHue + distance * t;
  // Ensure the interpolated hue is within the 0-359 range
  interpolatedHue = (interpolatedHue + 360) % 360;
  return interpolatedHue;
}

// finds the closest distance between a point and an array of points
function findClosestDistance(points, point) {
  let minDistance = Infinity;
  points.forEach(p => {
    const d = Math.sqrt(Math.pow(p.offsetX - point.offsetX, 2) + Math.pow(p.offsetY - point.offsetY, 2));
    if (d < minDistance) {
      minDistance = d;
    }
  });
  return minDistance;
}

// returns a range of values evenly spaced between a start and end value
function makeRangeArr(startValue, stopValue, cardinality) {
  var arr = [];
  var step = (stopValue - startValue) / (cardinality - 1);
  for (var i = 0; i < cardinality; i++) {
    arr.push(startValue + (step * i));
  }
  return arr;
}

function findCommonElements(coordsA, coordsB) {
  var commonElements = [];
  coordsA.forEach(function(elem1) {
      coordsB.forEach(function(elem2) {
          if (elem1[0] === elem2[0] && elem1[1] === elem2[1]) {
              commonElements.push(elem1);
          }
      });
  });
  return commonElements;
}
