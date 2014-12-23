var _ = require('underscore');
var assert = require('assert');
var debug = require('debug')('DEBUG');

// require('colors');

var context = {};
context.frames = [];
context.probes = 0;
context.possibleError = { frames:[] };

var _v = {}; // validators
var _s = {}; // sanitizer


// api
function validate(specs, input, cb) {
  var pass = _validate(specs, input);
  var err = null;

  if (pass) {
    assert.equal(context.frames.length, 0);
    assert.equal(context.probes, 0);
    assert.equal(context.possibleError.frames.length, 0);
  } else {
    // report error
    var err = reportError();    
  }

  if (cb) cb(err);
  else return pass;
}

// validating `input` using `specs`
function _validate(specs, input) {
  // assert.equal(input.type, 'list', ['Input should be a list']);
  var result = iterateSchema(input, function(item) {
    pushFrame('probe');
    var pass = _.some(specs, doValidate, {
      item: item
    });
    popFrame(pass);
    return pass;
  }, true);

  return result;
}

function doValidate(spec) {
  var out = sanitize(this.item);
  var fun = validator(spec.type);

  debug('validate', 'item:')
  debug(this.item);
  debug('validate', 'item.raw:', out);
  debug('using', 'spec:')
  debug(spec);

  assert.notEqual(fun, undefined);
  pushFrame(this.item, spec);

  debugBegin(spec.type[0]);
  var pass = fun.call(spec, this.item);
  debugEnd(spec.type[0], pass);

  popFrame(pass);

  return pass;
}


_v.validateTaggedValue = function(item) {
  var result = false;
  var checked =
    item.type === 'tuple' &&
    item.length === 2 &&
    item[0].type === 'atom' &&
    item[0][0] === this.tag[0];

  if (checked) {
    result = doValidate.call({
      item: item[1]
    }, this.val[0]);
  }

  return result;
}

_v.validateSimpleList = function(item) {
  var result = false;
  var checked =
    item.type === 'list' &&
    item.length >= 0

  if (checked) {
    if (item.length === 0) {
      result = true;
    } else {
      checked = this.eletype;// && this.eleval;
      if (checked) {
        this.type = this.eletype;
        this.val = this.eleval;
        delete this.eletype;
        delete this.eleval;
        result = _validate([this], item);
      }
    }
  }


  return result;
}

_v.validateList = function(item) {
  var result = false;
  var checked =
    item.type === 'list' &&
    item.length >= 0

  if (checked) {
    if (item.length === 0) {
      result = true;
    } else {
      result = _validate(this.val, item);
    }
  }

  return result;
}

_v.validateTuple = function(item) {
  var result = false;
  var checked =
    item.type === 'tuple' &&
    item.length === this.val.length;

  if (checked) {
    for (var i = 0; i < item.length; i++) {
      var done = false;
      if (this.val[i].val && !this.val[i].type){ // for simple-list element
        done = _validate(this.val[i].val, [item[i]]);
        // done = _.some(this.val[i].val, function(spec) {
        //   return doValidate.call({
        //     item: item[i]
        //   }, spec);
        // });
      } else {
        var done = doValidate.call({
          item: item[i]
        }, this.val[i]);
      }
      if (!done) {
        break;
      }
      if (i === (item.length - 1)) {
        result = true;
      }
    }
  }

  return result;
}

_v.validateInteger = function(item) {
  var result = typeof item[0] === 'number';

  return result;
}

_v.validateAtom = function(item) {
  var result = false;
  var checked = item.type === 'atom';

  if (checked) {
    if (this.altval) {
      result = _.some(this.altval, function(v) {
        return (v.type === 'atom') && (v[0] === item[0]);
      })
    } else {
      result = typeof item[0] === 'string';
    }
  }

  return result;
}

_v.validateBoolean = function(item) {
  var result = item.type === 'atom' &&
    item[0] === 'true' || item[0] === 'false';

  return result;
}

_v.validateString = function(item) {
  debug('start validating string..');
  var result = false;
  var checked = item.type === 'string' && item.length === 1;

  if (checked) {
    if (this.altval) {
      result = _.some(this.altval, function(v) {
        return (v.type === 'string') && (v[0] === item[0]);
      })
    } else {
      result = typeof item[0] === 'string';
    }
  }

  debug('end validating string..', result);
  return result;
}

function parse(schema) {
  var specs = [];

  assert.equal(typeof schema, 'object', ['Schema should be an object']);
  assert.equal(schema.length, 1);
  assert.equal(schema.type, 'list');
  assert.equal(typeof schema[0], 'object');

  schema = schema[0];
  assert.equal(typeof schema.length, 'number');
  assert.equal(schema.type, 'list');

  iterateSchema(schema, function(s) {
    specs.push(transform(s));
  });

  return specs;
}

function transform(schema) {
  assert.equal(typeof schema.length, 'number');
  assert.equal(schema.type, 'list');

  var term = {};

  iterateSchema(schema, parseTerm(term));

  return term;
}

function iterateSchema(schema, iterator, validate) {
  for (var i = 0; i < schema.length; i++) {
    assert.notEqual(schema[i], undefined);
    var done = iterator.call(null, schema[i]);

    if (!done && validate) { // return false if anyone fail
      return false;
    }
  }

  return true;
}

function parseTerm(term) {
  var iter = function(schema) {
    assert.equal(schema.length, 2);
    assert.equal(schema.type, 'tuple');
    assert.equal(schema[0].type, 'atom');

    var fun = parser(schema[0][0]);

    assert.notEqual(fun, undefined);

    fun.call(null, term, schema[1]);
  };

  return iter;
}

function parseTitle(term, value) {
  assert.equal(value.type, 'binstr');
  assert.equal(value.length, 1);
  assert.equal(typeof value[0], 'string');

  term.title = value[0];
}

function parseDesc(term, value) {
  assert.equal(value.type, 'binstr');
  assert.equal(value.length, 1);
  assert.equal(typeof value[0], 'string');

  term.desc = value[0];
}

function parseAltValue(term, value) {
  if (term.altval) {
    term.altval.push(value);
  } else {
    term.altval = [value];
  }
}

_s.sanitizeTuple = function(item, recursive) {
  var stash = [];

  for (var i=0; i<item.length; i++) {
    if (!recursive) {
      stash.push('_');
    } else {
      var fun = sanitizer(item[i].type);
      var part = fun.call(null, item[i], false);
      stash.push(part);
    }
  }

  var out = '{' + stash.join(', ') + '}';
  return out;
}

_s.sanitizeList = function(item, recursive) {
  var stash = [];
  var len = item.length;
  var mass = false;

  if (len > 5) {
    len = 3;
    mass = true;
  }

  for (var i=0; i<len; i++) {
    if (!recursive) {
      stash.push('_');
    } else {
      var fun = sanitizer(item[i].type);
      var part = fun.call(null, item[i], false);
      stash.push(part);
    }
  }

  if (mass) {
    stash.push('...(' + (item.length-len) + ')');
  }

  var out = '[' + stash.join(', ') + ']';
  return out;
}

_s.sanitizeInteger = function(item) {
  return item[0] + '';
}

_s.sanitizeAtom = function(item) {
  var atom;
  if (item.quoted) {
    atom = "'" + item[0] + "'";
  } else {
    atom = item[0];
  }

  return atom;
}

_s.sanitizeString = function(item) {
  return '"' + item[0] + '"';
}



// {attribute -> parser} map
function parser(key) {
  debug('get parser with key:', key);
  switch (key) {
    case '@type':
      return addPropGenerator('type');
    case '@tag':
      return addPropGenerator('tag');
    case '@title':
      return parseTitle;
    case '@desc':
      return parseDesc;
    case '@value':
      return parseValueGenerator('val');
    case '@alternative-value':
      return parseAltValue;
    case '@element-type':
      return addPropGenerator('eletype');
    case '@element-value':
      return parseValueGenerator('eleval');
    default:
      return undefined
  };
}

function unparse(ir) {
  var out = {type:'list'};
  var index = 0;

  ir.forEach(function(node) {
    var fun = unparser(node.type[0]);
    assert.notEqual(fun, undefined);
    var term = fun.call(null, node);    
    if (term) {
      out[index++] = term;
    }
  });

  out.length = index;

  return out;
}

function unparseTaggedValue(node) {
  debug('unparse tagged-value:', node); 
  var out = {};
  out.type = 'tuple';
  out.length = 2;
  out[0] = {
    0: node.tag[0],
    type: node.tag.type
  }
  out[1] = {};
  var index = 0;

  if (node.val[0].type[0] === 'simple-list') {
    if (node.val[0].value) {
      out[1].type = 'list';
      out[1].length = node.val[0].value.length;
      if (node.val[0].eletype[0] === 'string') {
        for (var i=0; i<out[1].length; i++) {
          out[1][i] = {
            0: node.val[0].value[i],
            type: 'string'
          };
        }
        return out;
      } else {
        node.val[0].value.forEach(function(node){
          var subType = node.type[0];
          var fun = unparser(subType);
          assert.notEqual(fun, undefined);
          var term = fun.call(null, node); 
          if (term) {
            out[1][index++] = term;
          }
        });
        out[1].length = index;
        return out;
      }
    }
  } else if (node.val[0].type[0] === 'list') {
    if (node.val[0].value) {
      out[1].type = 'list';
      node.val[0].value.forEach(function(node) {
        var subType = node.type[0];
        var fun = unparser(subType);
        assert.notEqual(fun, undefined);
        var term = fun.call(null, node); 
        if (term) {
          out[1][index++] = term;
        }
      });
      out[1].length = index;
      return out;
    }
  } else if (node.val[0].type[0] === 'tuple') {
    node.val[0].val.forEach(function(node) {
      var subType = node.type[0];
      var fun = unparser(subType);
      assert.notEqual(fun, undefined);
      var term = fun.call(null, node);
      if (term) {
        out[1][index++] = term;
      }
    });
    if (index) {
      out[1].type = 'tuple';
      out[1].length = index;
      return out;
    }
  } else if (node.val[0].type[0] === 'atom') {
    if (node.value) {
      out[1][0] = node.value[0];
      out[1].type = 'atom';
      return out;
    }
  } else if (node.val[0].type[0] === 'boolean') {
    // if (node.value) {
      out[1][0] = new Boolean(node.value); 
      out[1].type = 'boolean';
      return out;
    // }
  } else if (node.val[0].type[0] === 'string') {
    // if (node.value) {
      out[1][0] = node.value;
      out[1].type = 'string';
      return out;
    // }
  } else if (node.val[0].type[0] === 'integer') {
    // if (node.value) {
      out[1][0] = parseInt(node.value);
      out[1].type = 'integer';
      return out;
    // }
  } else {
    debug("** unexpected type:", node.val[0].type[0]);
  }

}

function unparseAtom(node) {
  if (node.value) {
    var out = {};
    out.type = 'atom';
    out[0] = node.value;
    return out;
  }
}

function unparseString(node) {
  var out = {};
  out.type = 'string';
  out[0] = node.value;
  return out;
}

function unparseInteger(node) {
  // if (node.value !== undefined) {
    var out = {};
    out.type = 'integer';
    out[0] = parseInt(node.value) || 0;
    return out;
  // }
}

function unparseBoolean(node) {
  // if (node.value !== undefined) {
    var out = {};
    out.type = 'boolean';
    out[0] = new Boolean(node.value);
    return out;
  // }
}

function unparseTuple(node) {
  var out = {};
  var index = 0;

  node.val.forEach(function(node) {
    if (node.type) {
      var fun = unparser(node.type[0]);
      assert.notEqual(fun, undefined);
      var term = fun.call(null, node);
      if (term) {
        out[index++] = term;
      }
    } else {
      var fun = unparser(node.value[0].type[0]);
      assert.notEqual(fun, undefined);
      var term = fun.call(null, node.value[0]);
      if (term) {
        out[index++] = term;
      }
    }
  });

  if (index) {
    out.type = 'tuple';
    out.length = index;
    return out;
  }

}

function unparseList(node) {
  var out = {};
  var index = 0;

  node.value && node.value.forEach(function(node) {
    var fun = unparser(node.type[0]);
    assert.notEqual(fun, undefined);
    var term = fun.call(null, node);
    if (term) {
      out[index++] = term;
    }
  });

  out.type = 'list';
  out.length = index;
  return out;
}

// {type -> unparser} map
function unparser(key) {
  debug('get unparser with key:', key);
  switch (key) {
    case 'tagged-value':
      return unparseTaggedValue;
    case 'list':
      return unparseList;
    case 'tuple':
      return unparseTuple;
    case 'string':
      return unparseString;
    case 'integer':
      return unparseInteger;
    case 'boolean':
      return unparseBoolean;
    case 'atom':
      return unparseAtom;
    default:
      return undefined
  };
}

function validator(key) {
  assert.equal(key.type, 'atom');
  key = key[0];
  debug('get validator with key:', key);

  key = key.split('-').map(function(k) {
    return k.charAt(0).toUpperCase() + k.slice(1);
  });
  key.unshift('validate');

  var fun = _v[key.join('')];

  assert(typeof fun, 'function');
  return fun;
}

function sanitize(item) {
  assert.notEqual(item.type, undefined);
  var fun = sanitizer(item.type);

  var out = {
    "line": item.line,
    "column": item.column,
    "raw": fun.call(null, item, true)
  }
  return out;
}

function sanitizer(type) {
  var fun= _s['sanitize' + capitalise(type)];

  assert.equal(typeof sanitizer, 'function');
  return fun;
}

function addPropGenerator(key) {
  return function(term, value) {
    term[key] = value;
  };
}

function parseValueGenerator(key) {
  var parser = function(term, value) {
    assert.equal(value.type, 'list');
    assert.equal(typeof value.length, 'number');

    if (term[key]) {
      term[key].push(transform(value));
    } else {
      term[key] = [transform(value)];
    }
  }

  return parser;
}

function capitalise(str)
{
  return str.charAt(0).toUpperCase() + str.slice(1);
}

function pushFrame(item, spec) {
  var frame = {
    "item": item,
    "spec": spec
  };

  if (item === 'probe') {
    context.probes++;
    context.possibleError.frames.push(frame);
  }
  context.frames.push(frame);
}

function popFrame(pass) {
  var frameSize = context.frames.length;
  var top = context.frames.pop();
  var topIsProbe = top && top.item === 'probe';

  if (pass) {
    if (topIsProbe) {
      // clear context.possibleError.frames to next probe
      while (true) {
        var errorFrame = context.possibleError.frames.pop();
        assert.notEqual(errorFrame, undefined);
        if (errorFrame.item === 'probe') {
          break;
        }
      }
    }
  } else {
    top.deepth = frameSize - context.probes;
    if (topIsProbe) {
      var maxFrames = [context.possibleError.frames.pop()];
      if (maxFrames[0].item !== 'probe') {
        while (true) {
          var errorFrame = context.possibleError.frames.pop();
          assert.notEqual(errorFrame, undefined);
          if (errorFrame.item === 'probe') {
            _.each(maxFrames, function(frame) {
              context.possibleError.frames.push(frame);
            })
            break;
          } else {
            if (errorFrame.deepth > maxFrames[0].deepth) {
              maxFrames = [errorFrame];
            } else if (errorFrame.deepth === maxFrames[0].deepth) {
              maxFrames.push(errorFrame);
            }
          }
        }
      }
    } else {
      context.possibleError.frames.push(top);      
    }
  }

  if (topIsProbe) {
    context.probes--;
  }
}

function topFrame(stack) {
  var top = stack[stack.length-1];

  return top;
}

function reportError() {
  var errorSize = context.possibleError.frames.length;
  assert.equal(errorSize > 0, true);
  var errorFrame = context.possibleError.frames.pop();
  var stain = sanitize(errorFrame.item);
  // var msg = ('Line ' + stain.line + ', Col ' + stain.column + ': ').red +
  //     stain.raw.grey + ', Should be:'.green;

  var err = {
    line: stain.line,
    column: stain.column,
    stain: stain.raw,
    forms: []
  }

  // console.log(msg);

  while (true) {
    if (!errorFrame) {
      break;
    }
    err.forms.push(errorFrame.spec);
    // console.log(errorFrame.spec);
    errorFrame = context.possibleError.frames.pop();
  }

  return err;
}

function debugBegin(type) {
  debug('start validating', type, '..');
}

function debugEnd(type, pass) {
  debug('end validating', type, '..');
}


exports.parse = parse;
exports.unparse = unparse;
exports.validate = validate;
