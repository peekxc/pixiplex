// pixinet.js 
// Contains utilities for making PIXI-driven force-directed network 

import * as PIXI from 'pixi.js';
import Viewport from 'pixi-viewport';
import { selection, select } from 'd3-selection';
import { scaleLinear } from 'd3-scale';
import { polygonContains } from 'd3-polygon';
import lasso from './lasso.js';
import { dispatch } from 'd3-dispatch';
import { forOwn, map, remove, concat, filter, unionBy, pullAllBy, pullAllWith, intersectionWith, unionWith, differenceBy, differenceWith, transform, includes, isFunction, isEmpty, merge, flatMap } from 'lodash-es';
import { forceCenter, forceCollide, forceLink, forceManyBody, forceRadial, forceSimulation, forceX, forceY } from 'd3-force';
import * as d3_force from 'd3-force';
import * as EventEmitter from 'eventemitter3';


export const combinations = (n, k) => {
  const result= [];
  const combos = [];
  const recurse = start => {
    if (combos.length + (n - start + 1) < k) { return }
    recurse(start + 1);
    combos.push(start);
		if(combos.length === k) { result.push(combos.slice()); }
		else if(combos.length + (n - start + 2) >= k){ recurse(start + 1); }
    combos.pop();     
  }
  recurse(1, combos);
  return result;
}

export const range = (arr, accessor) => { return [ minBy(arr, accessor), maxBy(arr, accessor)] }
export const identity = (val) => { return val; }
export const compose = (...fns) => fns.reduce((f, g) => (...args) => f(g(...args))) // why is this not standard...

// Makes linear scaling functions that take coordinates in [0, 1] -> [w, h] + inverse	
export const make_scale = (w, h) => {
	const scale_x = scaleLinear().domain([0, 1]).range([0, w]);
	const scale_y = scaleLinear().domain([0, 1]).range([0, h]);
	const scale_xy = (xy) => { return [scale_x(xy[0]), scale_y(xy[1])] }
	const invert_scale_xy = (xy) => { return [scale_x.invert(xy[0]), scale_x.invert(xy[1])] }
	return { scale: scale_xy, invert: invert_scale_xy };
}

const node_style = { 
	lineStyle: { size: 1.5, color: 0xFFFFFF },
	color: 0x650A5A,
	radius: 6,
	alpha: 1
}
const line_style = { lineWidth: 1, color: 0x000000, alpha: 1 }
const polygon_style = {
	lineStyle: { size: 1.5, color: 0xFFFFFF },
	color: 0x650A5A,
	alpha: 0.20
}
const px = PIXI.default;

// Parameter for all forces
let default_link_params = { 
	distance: 30,
	iterations: 1, 
	id: function(d){ return d.id; } 
};
let default_manybody_params = { strength: function() { return -30 }, distanceMin: 1, distanceMax: Infinity }
let default_center_params = { x: 0, y: 0 }
const default_sim_params = {
	alpha: 1, 
	force: { // < name > : { enabled: < boolean >, type: < force type >, params: { < force parameters > } }
	  charge: { enabled: true, type: "forceManyBody", params: default_manybody_params },
	  link: { enabled: true, type: "forceLink", params: default_link_params },
	  center: { enabled: true, type: "forceCenter", params: default_center_params }
	}
};
export const current_ns = (node) => {
	let gd = node.graphicsData[0]
	let c_ns = { 
		lineStyle: { size: gd.lineWidth, color: gd.lineColor },
		color: gd.fillColor,
		radius: gd.shape.radius,
		alpha: gd.fillAlpha
	};
	c_ns.lineStyle = clean(c_ns.lineStyle)
	if (isEmpty(c_ns.lineStyle)){ delete c_ns.lineStyle; }
	return clean(c_ns)
}
export const default_ns = (node) => {
	let c_ns = current_ns(node);
	let res = node_style;
	if ("alpha" in c_ns){ res.alpha = c_ns.alpha; }
	if ("color" in c_ns){ res.color = c_ns.color; }
	if ("radius" in c_ns){ res.radius = c_ns.radius; }
	if (!isEmpty(c_ns.lineStyle) && "size" in c_ns.lineStyle){ res.lineStyle.size = c_ns.lineStyle.size; }
	if (!isEmpty(c_ns.lineStyle) && "color" in c_ns.lineStyle){ res.lineStyle.color = c_ns.lineStyle.color; }
	return res;
}

export const clean = (obj) => {
  Object.keys(obj).forEach((key) => (obj[key] == null) && delete obj[key]);
  return obj;
}

export const register_shiny_input = (Shiny, name, value)=> {
	if (isFunction(value)){
		Shiny.onInputChange(name, value());
	} else {
		Shiny.onInputChange(name, value);
	}
}
  
// Applies simulation settings a d3 force simulation
export const apply_sim = (sim, params) => {
	forOwn(params, function(value, key){
		if (key != "force"){ 
			console.log(key.toString() + " = " + value.toString())
			sim[key](value); 
		} 
	});
	return sim
}

// Applies force settings a d3 force simulation
export const apply_force = (sim, params) => {
	forOwn(params, function(settings, forcename){
		if (settings.enabled){
			sim.force(forcename, d3_force[settings.type]()); // set up default force
			forOwn(settings.params, function(param_value, param_name){
				console.log(forcename.toString() + ": " + param_name.toString() + " = " + param_value.toString())
				sim.force(forcename)[param_name](param_value);
			})
		}
	})
	return sim
}

// Scales node .x, .y coordinates to the given width w and height h.
// If nodes have no intrinsic coordinates, they are given random positions.
export const scale_nodes = (nodes, w, h) => {
	let scale_f = make_scale(w, h);
	nodes.forEach((node) => { 
		if (!('x' in node)){ node.x = Math.random(); }
		if (!('y' in node)){ node.y = Math.random(); }
		let xy = scale_f.scale([node.x, node.y])
		node.x = xy[0]; node.y = xy[1];
	});
	return nodes;
}

export const ticker = (app, stage) => {
	let dispatcher = dispatch("tick", "animate", "stop", "restart");
	let end_loop = false; 
	function animate(){
		if (!end_loop) { requestAnimationFrame(animate); }
		app.renderer.render(stage);
		dispatcher.call("tick", this);
	}
	dispatcher.on('animate', animate);
	dispatcher.on('stop', function(){ 
		end_loop = true; 
		console.log('ticker stopped'); 
	});
	dispatcher.on('restart', function(){ 
		end_loop = false; 
		dispatcher.call('animate');
	});
	return(dispatcher);
}

export const clear_stage = (stage) => {
	for (var i = stage.children.length - 1; i >= 0; i--) {	
		stage.removeChild(stage.children[i]);
	};
}

// Create viewport
export const create_viewport = (app, sw, sh, ww=sw, wh=sh) => {
	var viewport = new Viewport({
		screenWidth: sw, screenHeight: sh,
		worldWidth: ww, worldHeight: wh,
		interaction: app.renderer.plugins.interaction 
	});
	return(viewport);
}

// Recreates node Graphics objects
export const generate_node_graphics = (nodes) => {
	return map(nodes, (node) => { return Object.assign(draw_node(new px.Graphics()), node); });
} 
// Generate link Graphics object
export const generate_links_graphic = () => { 
	return(new px.Graphics());
}
export const generate_polygon_graphics = (polygons) => {
	return _.map(polygons, (polygon) => { polygon.gfx = new px.Graphics(); return polygon; });
}

// Private helper; draws the actual circle
const _draw_node = (node, x, y, r) => {
	node.drawCircle(x, y, r);
}

// Simple function to color nodes w/ given style
export const draw_node = (node, ns = node_style) => {
	node.clear();
	node.lineStyle(ns.lineStyle.size, ns.lineStyle.color)
	node.beginFill(ns.color, ns.alpha);
	// node.drawCircle(0, 0, ns.radius);
	_draw_node(node, 0, 0, ns.radius);
	node.endFill();
	return(node)
}

// lineStyle: { size: 1.5, color: 0xFFFFFF },
// color: 0x650A5A,
// radius: 6,
// alpha: 1
// export const update_node_style = (gfx, ns) => {
// 	var len = gfx.graphicsData.length;    
//   for (var i = 0; i < len; i++) {        
//     var data = gfx.graphicsData[i];
//     data.lineWidth = ns.lineWidth;        
//     data.lineColor = ns.color;        
//     data.alpha = ns.alpha;   
//     gfx.dirty++;        
//     gfx.clearDirty++;    
//   }   
// }

// export const set_node_style = (nodes, ns = node_style) => {
// 	if (ns.constructor === Array && ns.length == nodes.length){
// 		nodes.forEach((node, i) => { draw_node(node, Object.assign(node_style, ns[i])) })
// 	} else if (ns.constructor == Object){
// 		nodes.forEach((node) => { draw_node(node, Object.assign(node_style, ns)) })
// 	}
// }

// Draws the nodes according to the current style; adds them to 
export const draw_nodes = (nodes, ns = node_style) => {
	if (ns.constructor === Array && ns.length == nodes.length){
		console.log(ns[0])
		console.log(default_ns(nodes[0]))
		console.log(merge(default_ns(nodes[0]), ns[0]))
		nodes.forEach((node, i) => { draw_node(node, merge(default_ns(node), ns[i])) })
	} else if (ns.constructor == Object){
		let ns_new = merge(default_ns(node), ns);
		nodes.forEach((node) => { draw_node(node, ns_new) })
	}
}

// Helper method; moves the link graphics to the link source coordinate, then draws
// the line with whatever style the link graphics is set with.
const _draw_link = (link, link_gfx) => {
	let { source, target } = link;
	link_gfx.moveTo(source.x, source.y);
	link_gfx.lineTo(target.x, target.y);
}

// Optionally overrides the line style, then draws a single link. Does not call endFill. 
export const draw_link = (link, link_gfx, ls = line_style) => {
	link_gfx.lineStyle(ls.lineWidth, ls.color);
	_draw_link(link, link_gfx);
	return(link)
}

// Draw Links according to a given styling, or default style otherwise
// If the supplied line style is an array, draw links with individual styles.
// otherwise if the supplied link style is an object, draws all links with that style.
export const draw_links = (links, link_gfx, ls = line_style) => {
	if (ls.constructor === Array && ls.length == links.length){
		links.forEach((link, i) => { draw_link(link, link_gfx, merge(line_style, ls[i])) });
		link_gfx.endFill();
	} // o.w. draw every link with specified style
	else if (ls.constructor == Object){
		let new_ls = merge(line_style, ls);
		link_gfx.clear();
		link_gfx.lineStyle(new_ls.lineWidth, new_ls.color);
		links.forEach((link) => { _draw_link(link, link_gfx) });
		link_gfx.endFill();
	}
}

// ------------ Polygon draw methods ------------
export const _draw_polygon = (polygon, poly_gfx) => {
	if (polygon.nodes){ polygon.points = flatMap(polygon.nodes, (node) => { return [node.x, node.y]; }); }
	poly_gfx.drawPolygon(polygon);
} 
export const draw_polygon = (polygon, poly_gfx, ps = polygon_style) => {
	poly_gfx.clear();
	poly_gfx.beginFill(ps.color);
	_draw_polygon(polygon, poly_gfx);
	poly_gfx.endFill();
}
export const draw_polygons = (polygons, ps = polygon_style) => {
	if (ps.constructor === Array && ps.length == polygons.length){
		polygons.forEach((poly, i) => { draw_polygon(poly, poly.gfx, Object.assign(polygon_style, ps[i])) });
	} // o.w. draw every polygon with specified style
	else if (ps.constructor == Object){
		polygons.forEach((poly) => { draw_polygon(poly, poly.gfx, ps) });
	}
}


export const register_tick_stops = (tick_dispatcher, vp, predicate = function(){ return true; }) => {
	vp.on('clicked', () => tick_dispatcher.call('restart'))
	vp.on('drag-start', () => tick_dispatcher.call('restart'))
	vp.on('pinch-start', () => tick_dispatcher.call('restart'))
	vp.on('moved', () => tick_dispatcher.call('restart'))
	vp.on('zoomed', () => 	tick_dispatcher.call('restart'))
	vp.on('moved-end', () => {
		if (predicate()){
			tick_dispatcher.call('stop')
		}
	}) // only stop if viewport is completely still
}

// Enables / Disables interactivity of Graphics items 
export const enable_interactive = (arr, acc = identity) => { arr.forEach((item) => { acc(item).interactive = true }) }
export const disable_interactive = (arr, acc = identity) => { arr.forEach((item) => { acc(item).interactive = true }) }


export const drag_dispatcher = (node) => {
	let dsp = dispatch("start", "end", "dragging");
	node.on('pointerdown', function(e){ dsp.call('start', node, e) });
	node.on('pointerup', function(e){ dsp.call('end', node, e) });
	node.on('pointermove', function(e){
		if (this.dragging){
			let coords = this.data.getLocalPosition(this.parent);
			dsp.call('dragging', node, e, coords);
		}
	});
	return dsp;
}

export const pixi_drag = (pixi_obj) => {
	if (!pixi_obj.interactive){ pixi_obj.interactive = true; }
	return function(dispatcher){
		dispatcher.on('start.pixi', function(e){
			if (this.parent.pausePlugin){ this.parent.pausePlugin("drag") };
			Object.assign(this, { data: e.data, alpha: 0.8, dragging: true });
		}).on('end.pixi', function(e){
			if (this.parent.resumePlugin){ this.parent.resumePlugin("drag") }
			Object.assign(this, { data: null, alpha: 1, dragging: false });
		}).on('dragging.pixi', function(e, coords){
			this.x = coords.x, this.y = coords.y;
		});
		return dispatcher;
	}
}

// Enable PIXI dragging for a given PIXI object. The object could be any DisplayObject, 
// e.g. a graphics object or a container. Returns a dispatcher which can be used to add  
// event listeners to the start, end, and during drag events 
// export const enable_drag = (dispatcher, pixi_obj) => {
// 	if (!pixi_obj.interactive){ pixi_obj.interactive = true; }
// 	let dispatcher = dispatch("start", "end", "dragging");
// 	pixi_obj.on('pointerdown', function(e){
// 		// ticker.call('restart');
// 		if (this.parent.pausePlugin){ this.parent.pausePlugin("drag") };
// 		Object.assign(this, { data: e.data, alpha: 0.8, dragging: true });
// 		dispatcher.call("start", this);
// 	}).on('pointerup', function(e){
// 		//console.log('pointerup' + e);
// 		// ticker.call('stop');
// 		if (this.parent.resumePlugin){ this.parent.resumePlugin("drag") }
// 		Object.assign(this, { data: null, alpha: 1, dragging: false });
// 		dispatcher.call("end", this);
// 	}).on('pointermove', function(){
// 		if (this.dragging) {
// 			let ncoords = this.data.getLocalPosition(this.parent);
// 			// console.log(this);
// 			this.x = ncoords.x, this.y = ncoords.y;
// 			dispatcher.call("dragging", this, ncoords);
// 		}
// 	});
// 	return(dispatcher);
// }

// Enable regular pixi dragging, but attaches standard force-based drag callbacks as well. 
export const force_drag = (sim) => {
	return function(dispatcher){
		dispatcher.on("start.force", function(e){ 
			sim.alphaTarget(0.3).restart();
			this.fx = this.x; this.fy = this.y; 
		}).on("dragging.force", function(e, coords){
			this.fx = coords.x; this.fy = coords.y; 
		}).on("end.force", function(e){
			if (!this.dragging) { sim.alphaTarget(0); }
			this.fx = null; this.fy = null; 
		});
		return dispatcher;
	}
}

export const shiny_drag = (Shiny) => {
	return function(dispatcher){
		dispatcher.on("start.shiny", function(e){ 
			console.log('node_selected');
			Shiny.onInputChange("node_selected", this.id);
		});
		return dispatcher;
	}
}

// ---- PIXI Application stuff ----

// Stage all items in 'arr' to 'stage'. Access elements w/ 'accessor'
// should only be called once, not on tick
export const stage_items = (stage, arr, acc = identity) => { 
	arr.forEach((item) => { stage.addChild(acc(item)) }); 
}

export const insert_nodes = (nodes, new_nodes) => {
	let ins_nodes = generate_node_graphics(differenceBy(new_nodes, nodes, 'id'));
	return concat(nodes, ins_nodes);
}

// ---- Updating node / link arrays -----
export const remove_nodes = (node_ids, nodes, links, stage) => {
	remove(links, (link) => {
		return (includes(node_ids, link.source.id) || includes(node_ids, link.target.id));
	});
	let removed_nodes = remove(nodes, (node) => { return includes(node_ids, node.id); });
	removed_nodes.forEach((node) => { stage.removeChild(node) });
}


// ---- Access and setting node/link properties ----

// Update node.gfx positions w/ force node coordinates
export const update_node_coords = (nodes) => {
	let i, n = nodes.length;
	for (i = 0; i < n; ++i) {
		nodes[i].gfx.x = nodes[i].x;
		nodes[i].gfx.y = nodes[i].y;
	}
}

export const make_group = (nodes) => {
	let container = new px.Container();
	nodes.forEach((node) => { container.addChild(node); });
	// container.interactive = true; 
	return(container);
}

// Creates a new d3 force simulation 
export const enable_force = () => { return forceSimulation() }

// Applies default settings to force simulation
export function default_force_settings(sim, app, nodes, links){
	// console.log("arg len: "+arguments.length);
	// console.log(sim);
	if (arguments.length < 4){
		return default_sim_params;
	} else {
		const parent = app.view.parentNode;
		const width = parent.clientWidth, height = parent.clientHeight;
		let center = [ width/2, height/2 ]
		sim.nodes(nodes)
		apply_sim(sim, default_sim_params)
		apply_force(sim, default_sim_params.force)
		sim.force('center').x(center[0]).y(center[1]);
		sim.force('link').links(links);
	}
}

export const enable_resize = (app, vp = null) => {
	app.renderer.autoResize = true;
	const parent = app.view.parentNode;
	const _resize = (w = parent.clientWidth, h = parent.clientHeight) => {
		console.log("calling resize");
		// if (vp){ vp.resize(w, h); }
		//app.renderer.resize(w, h);
	}
	return(_resize);
}

// Resolves link source and target nodes
export const resolve_links = (nodes, links) => {
	links.forEach((link) => {
		if (!(link.source instanceof PIXI.Graphics)){
			link.source = nodes.find((node) => { return node.id == link.source });
		}
		if (!(link.target instanceof PIXI.Graphics)){
			link.target = nodes.find((node) => { return node.id == link.target });
		}
	});
}

// Attach lasso to interaction SVG
export const enable_lasso = (visRootID) => {
	// const screenScale = window.devicePixelRatio || 1;
	let dispatcher = dispatch("start", "selected");

	const visRoot = select('#'+visRootID);
	// console.log(visRoot);
	var interaction_svg = visRoot.append('svg')
		.attr('id', 'selection_svg')
		.attr('width', visRoot.style("width"))
		.attr('height', visRoot.style("height"))
		.style('position', 'absolute')
		.style('top', 0)
		.style('left', 0)
		.style('display', 'none');

	// Make new lasso instance
	var lassoInstance = lasso();
	let local_nodes = null; 
	// console.log(lassoInstance);
	
	// Register handler for when the lasso is ended. Dispatch to 'selected'. 
	lassoInstance.on('end', function(lassoPolygon){
		interaction_svg.style('display', 'none');
		console.log(local_nodes);
		const selected_nodes = local_nodes.filter((node) => {
			let xy = [ node.x, node.y ];
			return polygonContains(lassoPolygon, xy);
		});
		local_nodes = null; 
		dispatcher.call("selected", this, selected_nodes);
	});

	// Register listener for 'start' on dispatcher
	dispatcher.on("start", function(nodes){
		local_nodes = nodes;
		interaction_svg.style('display', 'inline');
		lassoInstance(interaction_svg);
	});
	// lassoInstance.on('start', function(lassoPolygon){
	// 	// Reset node colors ? 
	// 	dispatcher.call("start");
	// })
	
	return(dispatcher);
}

// export const start_lasso = (nodes, svg_el) => {
// 	svg_el.style('display', 'inline');

// 	// Reset selected points when starting a new polygon
// 	const handleLassoStart = (lassoPolygon) => {
// 		// console.log(lassoPolygon);
// 		// highlight_nodes([]);
// 	}
// 	// Lasso end function
// 	const handleLassoEnd = (lassoPolygon) => {
// 		svg_el.style('display', 'none');
// 		const selected_nodes = nodes.filter((node) => {
// 			let xy = [ node.x, node.y ];
// 			return polygonContains(lassoPolygon, xy);
// 		});
// 		return(selected_nodes);
// 	}
// 	console.log(lasso)
// 	var lassoInstance = lasso().on('start', handleLassoStart).on('end', handleLassoEnd);
// 	lassoInstance(svg_el);		
// }
export const enable_drag_container = (container) => {
	container.interactive = true; 
	container.visible = true; 
	//console.log(container.getBounds());
	container.hitArea = container.getBounds();
	return(container);
}

// Puts a list of items accessed by 'acc' into a container
export const group_items = (items, acc = identity) => {
	let group = new px.Container();
	items.forEach((item) => { group.addChild(acc(item)); });
	return group;
}

export { PIXI }
export { select }
export { map, forOwn, remove, concat, filter, unionBy, unionWith, pullAllBy, pullAllWith, intersectionWith, differenceBy, differenceWith, transform, includes, isEmpty, merge, flatMap}

