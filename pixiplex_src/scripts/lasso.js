// Based off of https://bl.ocks.org/pbeshai/8008075f9ce771ee8be39e8c38907570

import { drag, dispatch, mouse } from 'd3';

function polygonToPath(polygon) {
  return `M${polygon.map(d => d.join(',')).join('L')}`;
}

function distance(pt1, pt2) {
  return Math.sqrt((pt2[0] - pt1[0]) ** 2 + (pt2[1] - pt1[1]) ** 2);
}

export default function lasso() {
  const d3_dispatch = dispatch('start', 'end');

  // distance last point has to be to first point before it auto closes when mouse is released
  let closeDistance = 75;

  function lasso(root) {
    // append a <g> with a rect
    const g = root.append('g').attr('class', 'lasso-group');
    const bbox = root.node().getBoundingClientRect();
    const area = g
      .append('rect')
      .attr('width', bbox.width)
      .attr('height', bbox.height)
      .attr('fill', 'tomato')
      .attr('opacity', 0);

    const d3_drag = drag()
      .on('start', handleDragStart)
      .on('drag', handleDrag)
      .on('end', handleDragEnd);

    area.call(d3_drag);

    let lassoPolygon;
    let lassoPath;
    let closePath;

    function handleDragStart() {
      lassoPolygon = [mouse(this)];
      if (lassoPath) { lassoPath.remove(); }

      lassoPath = g
        .append('path')
        .attr('fill', '#ababab')
        .attr('fill-opacity', 0.1)
        .attr('stroke', '#999999')
        .attr('stroke-dasharray', '3, 3');

      closePath = g
        .append('line')
        .attr('x2', lassoPolygon[0][0])
        .attr('y2', lassoPolygon[0][1])
        .attr('stroke', '#999999')
        .attr('stroke-dasharray', '3, 3')
        .attr('opacity', 0);

			d3_dispatch.call('start', lasso, lassoPolygon);
    }

    function handleDrag() {
      const point = mouse(this);
      lassoPolygon.push(point);
      lassoPath.attr('d', polygonToPath(lassoPolygon));

      // indicate if we are within closing distance
      if (
        distance(lassoPolygon[0], lassoPolygon[lassoPolygon.length - 1]) <
        closeDistance
      ) {
        closePath
          .attr('x1', point[0])
          .attr('y1', point[1])
          .attr('opacity', 1);
      } else {
        closePath.attr('opacity', 0);
      }
    }

    function handleDragEnd() {
      // remove the close path
      closePath.remove();
      closePath = null;

      // succesfully closed
      if (
        distance(lassoPolygon[0], lassoPolygon[lassoPolygon.length - 1]) <
        closeDistance
      ) {
        lassoPath.attr('d', polygonToPath(lassoPolygon) + 'Z');
        d3_dispatch.call('end', lasso, lassoPolygon);

        // otherwise cancel
      } else {
        lassoPath.remove();
        lassoPath = null;
        lassoPolygon = null;
      }
    }

    lasso.reset = () => {
      if (lassoPath) {
        lassoPath.remove();
        lassoPath = null;
      }

      lassoPolygon = null;
      if (closePath) {
        closePath.remove();
        closePath = null;
      }
    };
  }

  lasso.on = (type, callback) => {
    d3_dispatch.on(type, callback);
    return lasso;
  };

  return lasso;
}
