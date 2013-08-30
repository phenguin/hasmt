
var myCode = function () {

    var fbStartX = 50;
    var fbStartY = 50;

    var stage = new Kinetic.Stage({
        container : 'container', 
        width : 1575, 
        height : 400 
    });

    var layer = new Kinetic.Layer();

    var rect = new Kinetic.Rect({
        x :239, 
        y : 75, 
        width : 100,
        height : 50,
        stroke : 'black',
        strokeWidth : 4
    });

    layer.add(rect);

    stage.add(layer);

};

myCode();
