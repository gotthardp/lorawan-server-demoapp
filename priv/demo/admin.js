var myApp = angular.module('myApp');

function addPrivateMenu(nga, admin, dashL, dashR) {
    var motes = nga.entity('motes')
        .identifier(nga.field('devaddr'));

    // ---- motes
    motes.listView().fields([
        nga.field('devaddr').label('DevAddr').isDetailLink(true),
        nga.field('light'),
        nga.field('temp')
    ]);
    // add to the admin application
    admin.addEntity(motes);

    admin.menu()
        .addChild(nga.menu(motes).icon('<span class="fa fa-map-marker fa-fw"></span>'));

}

// end of file
