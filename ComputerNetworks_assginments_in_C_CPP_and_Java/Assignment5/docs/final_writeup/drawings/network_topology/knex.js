var redraw;

window.onload = function() {
    var width = 800;
    var height = 600;

    var render = function(r, n) {
            /* the Raphael set is obligatory, containing all you want to display */
            var set = r.set().push(
                /* custom objects go here */
                r.rect(n.point[0]-30, n.point[1]-13, 60, 44).attr({"fill": "#feb", r : "12px", "stroke-width" : n.distance == 0 ? "3px" : "1px" })).push(
                r.text(n.point[0], n.point[1] + 10, (n.label || n.id) + "\n(" + (n.distance == undefined ? "Infinity" : n.distance) + ")"));
            return set;
        };
    
    var g = new Graph();
    
    /* creating nodes and passing the new renderer function to overwrite the default one */
    g.addNode("cycle1", {render:render});
    g.addNode("cycle2", {render:render});
    g.addNode("cycle3", {render:render});

    g.addNode("f13", {render:render});
    g.addNode("f14", {render:render});
    g.addNode("f15", {render:render});
    g.addNode("f16", {render:render});
    g.addNode("f17", {render:render});
    g.addNode("f18", {render:render});
    g.addNode("f19", {render:render});

    /* connections */
    g.addEdge("cycle1", "cycle2", {label:6});
    g.addEdge("cycle1", "f13", {label:10});

    g.addEdge("cycle2", "f13", {label:5});
    g.addEdge("cycle2", "f14", {label:3});

    g.addEdge("cycle3", "f14", {label:12});
    g.addEdge("cycle3", "f15", {label:11});
    g.addEdge("cycle3", "f16", {label:2});

    g.addEdge("f13", "f17", {label:4});

    g.addEdge("f14", "f15", {label:4});
    g.addEdge("f14", "f17", {label:8});

    g.addEdge("f15", "f18", {label:7});
    g.addEdge("f15", "f19", {label:5});

    g.addEdge("f17", "f18", {label:6});

    g.addEdge("f18", "f19", {label:3});

    /* layout the graph using the Spring layout implementation */
    var layouter = new Graph.Layout.Spring(g);    
    var renderer = new Graph.Renderer.Raphael('canvas', g, width, height);

    redraw = function() {
        layouter.layout();
        renderer.draw();
    };
};
