import { task, src, dest, series, watch, parallel } from 'gulp'
import pug from 'gulp-pug';
import rollup from 'gulp-better-rollup';
import resolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import babel from 'rollup-plugin-babel';
import builtins from 'rollup-plugin-node-builtins';
import globals from 'rollup-plugin-node-globals';
import rename from 'gulp-rename';
import browserSync from 'browser-sync';
import { default as uglify } from 'gulp-uglify-es';
// import * as PIXI from 'pixi.js'

const html = () => {
	return src('templates/*.pug')
		.pipe(pug())
		.pipe(dest('.'))
}
task('pug', html)

// Do not change order of rollup plugins
const bundle = () => {
	process.env.NODE_ENV = "release";
	return src("scripts/pixinet.js")
		.pipe(rollup({
			plugins: [ 
				resolve({ jsnext: true, preferBuiltins: true }), 
				builtins(),
				babel({ babelrc: true }),
				commonjs({ namedExports: { 'pixi.js': [ 'VERSION', 'Application', 'Graphics' ] } }),
				globals()
			]
		}, { moduleName: 'PixiNet', file: 'pixinet.js', format: 'iife' })) // building for the browser
		.pipe(uglify())
		.pipe(rename('pixiplex.min.js'))
		.pipe(dest('.'))
		.pipe(dest('../inst/htmlwidgets/lib/pixiplex'))
		
}
task('bundle', bundle)

// Main task to develop the code
task('default', () => { series(bundle) })
task('dev', () => {
	browserSync.init({ server: { baseDir: "." } });
	watch("*.html").on('change', browserSync.reload); 
	watch("templates/*.pug").on('change', series(html, browserSync.reload))
	watch("scripts/*.js").on('change', series(bundle, html, browserSync.reload))
})