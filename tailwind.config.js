module.exports = {
  mode: 'jit',
  content: [
     // './resources/**/*.html',
     // './resources/**/*.js',
     './src/gamething/game.cljs',
     // './resources/**/*.js',
     'index.html',
     './js/app.js',
     // './resources/public/app.js'
  ],
  // purge: [
  //   './resources/**/*.html',
  //   './resources/**/*.js',
  //   './src/**/*.cljs'
  // ],
  theme: {
    extend: {},
  },
  variants: {},
  plugins: [],
}
