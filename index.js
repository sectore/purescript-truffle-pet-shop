import Main from './output/Main';

/*
Hot reloading a Halogen app
https://www.qwan.eu/2018/02/07/halogen-parceljs.html
and
https://github.com/justinwoo/halogen-parcel-hot-reload-demo/blob/master/index.js
*/
if (module.hot) {
  module.hot.accept( () => {
    // delete everything of previous Halogen app
    const el = document.getElementById('app');
    el.innerHTML = '';
    // ... to re-run app again
    Main.main();
  });
  // run app
  Main.main();
} else {
  Main.main();
}
