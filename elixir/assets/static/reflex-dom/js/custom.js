$(document).ready(function(){
  loadDeps();
  configureMaterializeAutoInit();
  loadReflex();
});

function loadReflex() {
  var script = document.createElement("script");  // create a script DOM node
  script.src = "/reflex-dom/js/runmain.js";  // set its src to the provided URL
  document.head.appendChild(script);
}

function loadDeps() {
  loadDep("/materialize/materialize.min.js");
  loadDep("/markdown/markdown.js");
  loadDep("/markdown/marked.js");
}

function loadDep(s) {
  var script = document.createElement("script");  // create a script DOM node
  script.src = s;  // set its src to the provided URL
  document.head.appendChild(script);
}

/* pair sidenav with edge-right or edge-left, edge-right is default */
/* pair dropdown-trigger with dd-hover for "open on hover" */

function configureMaterializeAutoInit() {
  var setupDD = function(el) {
    if(element.classList.contains("dd-hover")) {
      M.Dropdown.init(el, {hover: true});
    } else {
      M.Dropdown.init(el);
    }
  };

  var setupTB = function(el) {
    M.Tabs.init(el);
  };

  var setupSN = function(el) {
    if(el.classList.contains("edge-left")) {
      M.Sidenav.init(el, { edge: "left" });
    } else {
      M.Sidenav.init(el, { edge: "right" });
    }
  };

  var setupCP = function(el) {
    M.Collapsible.init(el);
  }

  var target = document.querySelector('body');

  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      mutation.addedNodes.forEach(function(element) {
        // Check Self
        if (element.classList) {
          if(element.classList.contains("dropdown-trigger")) {
            setupDD(element);
            console.log("Initializing Dropdown");
          }
          if(element.classList.contains("tabs")) {
            setupTB(element);
            console.log("Initializing Tabs");
          }
          if(element.classList.contains("sidenav")) {
            setupSN(element);
            console.log("Initializing Side Nav");
          }
          if(element.classList.contains("collapsible")) {
            setupCP(element);
            console.log("Initializing Collapsible");
          }
        }
        // Check Children
        if (element.getElementsByClassName) {
          var childDD = element.getElementsByClassName("dropdown-trigger");
          var childTB = element.getElementsByClassName("tabs");
          var childSN = element.getElementsByClassName("sidenav");
          var childCP = element.getElementsByClassName("collapsible");
          for(var i=0; i < childDD.length; i++) {
            setupDD(childDD[i]);
            console.log("Initializing Child Dropdown");
          }
          for(var i=0; i < childTB.length; i++) {
            setupTB(childTB[i]);
            console.log("Initializing Child Tabs");
          }
          for(var i=0; i < childSN.length; i++) {
            setupSN(childSN[i]);
            console.log("Initializing Child Side Nav");
          }
          for(var i=0; i < childCP.length; i++) {
            setupCP(childCP[i]);
            console.log("Initializing Child Collapsible");
          }
        }
      })
    });
  });

  // configuration of the observer:
  var config = { childList: true, subtree: true }

  // pass in the target node, as well as the observer options
  observer.observe(target, config);
}

function autoFocus(fltr, cnt) {
  var el = $(fltr);
  if(el.length) {
    el.focus();
  } else {
    if(cnt > 0) {
      setTimeout(function(){ autoFocus(fltr, cnt - 1); }, 0);
    }
  }
}
