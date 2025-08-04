// custom.js (versión final)

$(document).ready(function() {

    // Maneja el mensaje enviado desde Shiny (con los datos del árbol)
    Shiny.addCustomMessageHandler('jstree_data', function(message) {
        
        // Inicializa jstree en el contenedor con ID "jstree_demo"
        $('#jstree_demo').jstree({
            'core': {
                'data': message.data, 
                "themes" : {
                    "icons" : false
                },
                // Mantenemos esta propiedad, aunque no es suficiente por sí sola
                "open_all": false
            },
            "plugins": ["checkbox"]
        });
        
        // Adjuntamos el evento 'ready.jstree' después de la inicialización
        $('#jstree_demo').on('ready.jstree', function () {
            // Verifica si hay nodos para seleccionar por defecto
            if (message.default_selected && message.default_selected.length > 0) {
                // 1. Selecciona los nodos con los IDs proporcionados
                $(this).jstree(true).select_node(message.default_selected);
            }
            // 2. Colapsa todos los nodos del árbol, asegurando que los de nivel superior se cierren
            $(this).jstree(true).close_all();
        });
    });

    // Escucha el evento de "selección de nodos" de jstree
    $('#jstree_demo').on("changed.jstree", function (e, data) {
        
        // Obtiene los IDs de los nodos seleccionados
        var selected_nodes = data.selected;
        
        // Envía los IDs seleccionados a Shiny
        Shiny.setInputValue("selected_nodes", JSON.stringify(selected_nodes));
    });
});