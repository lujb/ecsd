%% Include File.
%%

-record(schema, {
    namespace,
    annotation,
    elements,
    location
    }).

-record(annotation, {
    name,
    docstring,
    type,
    use,
    repeat,
    onlywhen,
    tag,
    value
    }).

-record(element, {
    annotation,
    children=[], 
    value
    }).

%% Value Type
-record(oneof, {
    type = constant,
    children = []
    }).