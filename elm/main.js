(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.R.y === region._.y)
	{
		return 'on line ' + region.R.y;
	}
	return 'on lines ' + region.R.y + ' through ' + region._.y;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.a2,
		impl.bw,
		impl.bp,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_enqueueEffects(managers, result.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		n: func(record.n),
		S: record.S,
		P: record.P
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.n;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.S;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.P) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.a2,
		impl.bw,
		impl.bp,
		function(sendToApp, initialModel) {
			var view = impl.bx;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.a2,
		impl.bw,
		impl.bp,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.Q && impl.Q(sendToApp)
			var view = impl.bx;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aO);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.br) && (_VirtualDom_doc.title = title = doc.br);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.a9;
	var onUrlRequest = impl.ba;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		Q: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.ar === next.ar
							&& curr.af === next.af
							&& curr.ao.a === next.ao.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		a2: function(flags)
		{
			return A3(impl.a2, flags, _Browser_getUrl(), key);
		},
		bx: impl.bx,
		bw: impl.bw,
		bp: impl.bp
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { a$: 'hidden', aQ: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { a$: 'mozHidden', aQ: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { a$: 'msHidden', aQ: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { a$: 'webkitHidden', aQ: 'webkitvisibilitychange' }
		: { a$: 'hidden', aQ: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		ay: _Browser_getScene(),
		aG: {
			aI: _Browser_window.pageXOffset,
			aJ: _Browser_window.pageYOffset,
			aH: _Browser_doc.documentElement.clientWidth,
			ae: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		aH: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		ae: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			ay: {
				aH: node.scrollWidth,
				ae: node.scrollHeight
			},
			aG: {
				aI: node.scrollLeft,
				aJ: node.scrollTop,
				aH: node.clientWidth,
				ae: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			ay: _Browser_getScene(),
			aG: {
				aI: x,
				aJ: y,
				aH: _Browser_doc.documentElement.clientWidth,
				ae: _Browser_doc.documentElement.clientHeight
			},
			aU: {
				aI: x + rect.left,
				aJ: y + rect.top,
				aH: rect.width,
				ae: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.a7) { flags += 'm'; }
	if (options.aP) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.a) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.c),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.c);
		} else {
			var treeLen = builder.a * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.d) : builder.d;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.a);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.c) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.c);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{d: nodeList, a: (len / $elm$core$Array$branchFactor) | 0, c: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {ac: fragment, af: host, am: path, ao: port_, ar: protocol, at: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $author$project$Update$getDataPoint = function (_v0) {
	var uni = _v0.aE;
	var min = _v0.aj;
	var value = _v0.aF;
	var max = _v0.ai;
	return _Utils_Tuple2(
		uni,
		_Utils_Tuple3(min, value, max));
};
var $elm_community$basics_extra$Basics$Extra$fractionalModBy = F2(
	function (modulus, x) {
		return x - (modulus * $elm$core$Basics$floor(x / modulus));
	});
var $author$project$Update$unwrap = function (i) {
	return (!i) ? 100 : i;
};
var $author$project$Update$intToNss = function (i) {
	return {
		ai: $author$project$Update$unwrap(
			$elm$core$Basics$floor(
				A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 100, i))),
		aj: $author$project$Update$unwrap(
			$elm$core$Basics$floor(
				A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 100, i / 10000))),
		as: $elm$core$Basics$floor(
			A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 100, i / 1000000)),
		aE: $elm$core$Basics$floor(
			A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 1000, i / 100000000)),
		aF: $author$project$Update$unwrap(
			$elm$core$Basics$floor(
				A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 100, i / 100)))
	};
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $author$project$Update$matching = F3(
	function (qs, unis, _v0) {
		var q = _v0.as;
		var uni = _v0.aE;
		return A2($elm$core$Set$member, q, qs) && A2($elm$core$Set$member, uni, unis);
	});
var $author$project$Data$nss = _List_fromArray(
	[11601909293, 11602818385, 11603848688, 11604788083, 11605828486, 11606818385, 11607727477, 11608656770, 11609727577, 11610586163, 11611646769, 11612888991, 11613788082, 11614697174, 11615727476, 11616838587, 11617808284, 11618838587, 11619899192, 11620919294, 11621677072, 11622838587, 11623899092, 11624747678, 11625545659, 11626454851, 11627858788, 201929496, 202868992, 203869092, 204838790, 205868992, 206858992, 207838790, 208808487, 209778185, 210757983, 211818589, 212889193, 213848891, 214788286, 215747882, 216717680, 217808487, 218818588, 219828689, 220818588, 221667075, 222838790, 223889194, 224808487, 225636872, 226485358, 227868992, 301939496, 302878991, 303889092, 304828587, 305878991, 306878991, 307868890, 308808385, 309808385, 310818385, 311828486, 312939596, 313838688, 314808285, 315828587, 316798284, 317889092, 318929495, 319919394, 320919394, 321747679, 322889092, 323909294, 324848688, 325778082, 326606366, 327889092, 101778185, 102707580, 103596470, 104566267, 105667176, 106646974, 107636974, 108495460, 109495561, 110455156, 111546065, 112646974, 113576368, 114606671, 115263137, 116515662, 117424753, 118434854, 119485459, 120556166, 121546065, 122697479, 123636974, 124616772, 125414652, 126323844, 127556066, 401878889, 402838486, 403828385, 404798182, 405818284, 406838586, 407808283, 408747577, 409707273, 410747677, 411737576, 412818384, 413767879, 414747677, 415676970, 416788081, 417737576, 418808283, 419858688, 420838485, 421697072, 422858687, 423858688, 424757779, 425636566, 426596163, 427808283, 501788488, 502707681, 503818690, 504808589, 505818690, 506838892, 507808690, 508707681, 509626975, 510667278, 511788388, 512758186, 513667278, 514677479, 515616874, 516717783, 517677379, 518647176, 519556268, 520616874, 521505763, 522536066, 523667378, 524586470, 525485561, 526485561, 527758185, 601899193, 602899293, 603848789, 604848789, 605878991, 606858890, 607909394, 608737780, 609747780, 610828588, 611788184, 612878991, 613838689, 614838688, 615677174, 616747881, 617788284, 618858890, 619909294, 620879092, 621798285, 622818487, 623899193, 624808386, 625697276, 626646771, 627858890, 15601828485, 15602767879, 15603697173, 15604717375, 15605777981, 15606808183, 15607757778, 15608687072, 15609697173, 15610798183, 15611767879, 15612818284, 15613727476, 15614717374, 15615495153, 15616656769, 15617676970, 15618717375, 15619909293, 15620798182, 15621606264, 15622747677, 15623737577, 15624737476, 15625565860, 15626485052, 15627697173, 701868890, 702737578, 703848688, 704798183, 705798183, 706838586, 707777981, 708717476, 709697173, 710656870, 711646669, 712868889, 713737678, 714666971, 715677072, 716727477, 717757779, 718828486, 719838587, 720878990, 721606365, 722838587, 723878990, 724747779, 725646769, 726565961, 727818385, 901889092, 902838587, 903808284, 904777981, 905818385, 906838587, 907798183, 908747779, 909757780, 910697174, 911747679, 912858789, 913778082, 914767881, 915677073, 916727578, 917747779, 918838587, 919878991, 920878991, 921697274, 922828486, 923848688, 924798183, 925626467, 926606366, 927838587, 8601919394, 8602828486, 8603889091, 8604798183, 8605838586, 8606838586, 8607767880, 8608666870, 8609707274, 8610626567, 8611586062, 8612899092, 8613777980, 8614717476, 8615788081, 8616818385, 8617818385, 8618828485, 8619848687, 8620878990, 8621646668, 8622868789, 8623899092, 8624737577, 8625596163, 8626565861, 8627878890, 1001848789, 1002808385, 1003767981, 1004747780, 1005808385, 1006798284, 1007778083, 1008677072, 1009697275, 1010636769, 1011717477, 1012828587, 1013707376, 1014687174, 1015555861, 1016747779, 1017656871, 1018737578, 1019808385, 1020798184, 1021636669, 1022818385, 1023788083, 1024727578, 1025525558, 1026505357, 1027757881, 11701808284, 11702767981, 11703788183, 11704777982, 11705777982, 11706818486, 11707798184, 11708687174, 11709666871, 11710666972, 11711707375, 11712767981, 11713727477, 11714707275, 11715555861, 11716737678, 11717666972, 11718778082, 11719828486, 11720788183, 11721636669, 11722808385, 11723767981, 11724677072, 11725545760, 11726505356, 11727717477, 801858890, 802778083, 803677073, 804636770, 805757881, 806767982, 807717477, 808677174, 809677074, 810767982, 811697376, 812838588, 813757881, 814778083, 815555962, 816687275, 817717578, 818687275, 819586165, 820697275, 821606367, 822677073, 823747880, 824757880, 825555862, 826525660, 827687174, 1101879092, 1102808487, 1103879092, 1104757882, 1105808487, 1106818587, 1107727579, 1108687276, 1109717578, 1110626670, 1111717578, 1112778184, 1113697377, 1114656973, 1115667074, 1116818487, 1117778084, 1118778083, 1119828588, 1120818487, 1121485257, 1122667074, 1123798386, 1124677175, 1125475256, 1126404448, 1127798386, 8701899091, 8702808283, 8703878889, 8704777980, 8705838485, 8706818384, 8707757778, 8708626465, 8709656668, 8710636567, 8711626466, 8712828485, 8713707273, 8714676970, 8715626466, 8716707173, 8717707273, 8718818283, 8719878889, 8720888990, 8721626465, 8722808283, 8723818283, 8724687072, 8725505153, 8726495153, 8727818384, 11201899193, 11202798285, 11203757881, 11204788184, 11205818486, 11206848689, 11207818486, 11208737679, 11209687174, 11210798285, 11211808386, 11212838588, 11213808386, 11214757881, 11215707376, 11216747780, 11217727578, 11218808385, 11219858890, 11220848689, 11221677073, 11222838688, 11223808386, 11224757881, 11225646770, 11226545861, 11227818486, 1201868788, 1202808182, 1203808283, 1204798081, 1205838485, 1206838485, 1207848586, 1208757678, 1209717374, 1210767879, 1211747577, 1212818284, 1213777980, 1214757678, 1215656668, 1216717274, 1217757678, 1218798182, 1219858687, 1220858788, 1221676970, 1222858688, 1223838485, 1224747577, 1225606264, 1226555658, 1227798082, 1301869093, 1302838790, 1303788386, 1304788387, 1305848891, 1306869093, 1307828790, 1308687478, 1309747983, 1310828790, 1311737883, 1312828690, 1313808488, 1314818689, 1315647075, 1316727782, 1317717681, 1318808589, 1319879194, 1320848891, 1321707580, 1322869093, 1323828790, 1324778286, 1325667176, 1326556166, 1327818589, 1401879093, 1402838790, 1403838790, 1404848891, 1405838790, 1406869092, 1407838790, 1408778286, 1409818689, 1410768084, 1411798487, 1412838790, 1413818589, 1414808488, 1415677176, 1416818589, 1417727681, 1418768185, 1419768084, 1420768084, 1421757983, 1422838790, 1423818588, 1424788386, 1425677176, 1426606570, 1427808487, 8801909294, 8802838689, 8803879092, 8804868991, 8805868991, 8806858891, 8807848890, 8808778184, 8809747881, 8810737780, 8811798386, 8812838689, 8813828588, 8814828588, 8815656973, 8816828688, 8817717578, 8818848790, 8819858890, 8820858891, 8821757882, 8822858891, 8823838689, 8824757982, 8825667073, 8826626670, 8827818487, 1501878889, 1502798183, 1503798183, 1504788081, 1505828486, 1506828485, 1507798182, 1508697173, 1509697173, 1510697173, 1511697173, 1512838586, 1513747678, 1514677071, 1515616466, 1516747677, 1517707274, 1518828385, 1519868889, 1520858687, 1521656769, 1522858688, 1523828385, 1524717375, 1525535557, 1526586062, 1527788082, 8901838587, 8902727477, 8903808385, 8904788082, 8905778082, 8906808385, 8907808385, 8908656871, 8909646770, 8910646770, 8911697174, 8912788083, 8913717476, 8914697275, 8915596164, 8916717477, 8917687174, 8918808284, 8919899192, 8920868890, 8921677073, 8922858789, 8923798183, 8924707375, 8925596164, 8926606366, 8927757780, 1601808589, 1602737984, 1603687479, 1604778287, 1605778387, 1606818690, 1607808589, 1608707581, 1609717681, 1610667277, 1611737984, 1612727883, 1613747984, 1614717782, 1615535965, 1616727883, 1617606672, 1618505662, 1619677379, 1620606672, 1621657277, 1622798589, 1623798488, 1624758085, 1625566268, 1626525865, 1627697580, 11801868789, 11802808183, 11803798082, 11804747677, 11805798183, 11806818384, 11807777880, 11808656769, 11809676971, 11810656769, 11811717375, 11812818284, 11813727475, 11814707274, 11815525456, 11816737576, 11817666870, 11818777980, 11819888990, 11820838486, 11821636567, 11822798182, 11823777980, 11824717375, 11825525557, 11826464951, 11827747677, 11901899091, 11902818384, 11903889091, 11904767779, 11905818385, 11906808183, 11907747677, 11908626466, 11909676971, 11910626466, 11911626466, 11912848687, 11913717375, 11914666869, 11915626466, 11916798182, 11917727375, 11918818384, 11919858788, 11920878990, 11921626466, 11922757779, 11923838586, 11924707274, 11925565860, 11926394143, 11927808283, 1701828486, 1702717476, 1703767880, 1704737577, 1705767981, 1706777981, 1707737678, 1708636668, 1709626567, 1710606365, 1711626567, 1712848687, 1713717375, 1714666971, 1715646668, 1716737577, 1717747779, 1718777981, 1719828486, 1720818385, 1721586163, 1722788082, 1723808284, 1724697274, 1725565861, 1726515456, 1727757880, 9001879194, 9002798488, 9003828791, 9004798589, 9005788488, 9006838891, 9007818689, 9008626874, 9009737883, 9010626873, 9011677378, 9012889295, 9013818689, 9014747984, 9015717781, 9016758085, 9017747984, 9018758185, 9019778387, 9020798488, 9021727883, 9022838891, 9023858993, 9024768186, 9025616773, 9026495662, 9027838891, 1801879091, 1802828587, 1803818486, 1804828587, 1805848688, 1806868890, 1807858890, 1808737679, 1809677073, 1810747780, 1811798284, 1812788184, 1813778083, 1814788184, 1815636669, 1816788183, 1817707376, 1818788184, 1819818487, 1820828587, 1821737678, 1822858789, 1823848789, 1824788183, 1825666972, 1826707376, 1827818386, 1901868890, 1902818385, 1903808284, 1904798183, 1905828486, 1906838587, 1907828485, 1908717476, 1909677072, 1910687173, 1911737678, 1912788183, 1913747678, 1914707375, 1915525457, 1916687173, 1917606366, 1918818385, 1919889091, 1920858788, 1921656870, 1922818385, 1923818385, 1924707375, 1925515356, 1926505355, 1927757780, 2001889091, 2002808284, 2003808385, 2004788183, 2005828486, 2006848688, 2007828486, 2008697174, 2009697274, 2010777981, 2011717476, 2012848688, 2013788082, 2014767880, 2015707375, 2016747779, 2017788183, 2018848688, 2019888991, 2020868890, 2021717375, 2022858789, 2023818385, 2024777981, 2025666871, 2026535659, 2027838586, 2101878889, 2102798082, 2103848586, 2104747677, 2105808283, 2106828385, 2107777880, 2108676971, 2109666869, 2110676870, 2111636566, 2112858687, 2113727475, 2114687072, 2115596062, 2116747677, 2117727475, 2118818284, 2119899091, 2120878889, 2121626466, 2122798182, 2123828385, 2124717274, 2125525455, 2126626465, 2127808183, 12001838486, 12002767880, 12003788081, 12004767880, 12005808284, 12006808284, 12007808284, 12008697173, 12009707274, 12010757779, 12011737577, 12012818385, 12013747678, 12014727476, 12015596163, 12016727476, 12017707274, 12018828485, 12019878890, 12020858788, 12021687073, 12022848687, 12023798182, 12024717375, 12025586062, 12026586163, 12027747678, 12101889091, 12102828486, 12103818384, 12104798183, 12105858788, 12106858688, 12107838586, 12108737577, 12109747678, 12110818385, 12111757779, 12112848587, 12113808283, 12114767880, 12115697174, 12116737577, 12117747678, 12118868789, 12119899192, 12120889091, 12121687173, 12122848687, 12123878990, 12124788082, 12125636668, 12126525557, 12127858688, 9101899294, 9102868891, 9103808385, 9104808386, 9105858890, 9106828588, 9107838689, 9108697376, 9109687275, 9110788184, 9111798285, 9112878992, 9113818486, 9114808386, 9115646771, 9116778083, 9117727579, 9118848789, 9119899193, 9120858890, 9121767982, 9122889193, 9123818486, 9124778083, 9125646871, 9126565963, 9127828587, 2201828486, 2202687173, 2203767880, 2204707274, 2205707275, 2206747779, 2207707375, 2208606365, 2209606365, 2210646769, 2211586163, 2212828486, 2213687173, 2214646669, 2215606365, 2216687072, 2217707274, 2218828486, 2219838587, 2220858788, 2221545659, 2222798183, 2223778082, 2224666871, 2225525457, 2226525557, 2227727476, 7901909597, 7902889396, 7903879296, 7904869295, 7905849094, 7906828893, 7907909598, 7908637178, 7909657379, 7910637178, 7911788590, 7912909597, 7913818792, 7914778489, 7915616976, 7916707783, 7917687582, 7918677581, 7919768388, 7920808791, 7921869295, 7922929799, 7923859194, 7924778489, 7925637178, 7926667581, 7927879296, 2301899091, 2302838485, 2303858687, 2304828384, 2305858687, 2306868788, 2307848687, 2308757778, 2309717374, 2310737476, 2311747577, 2312868788, 2313818284, 2314787980, 2315707173, 2316747677, 2317788081, 2318858687, 2319899090, 2320888990, 2321737576, 2322899090, 2323868788, 2324787981, 2325666869, 2326616264, 2327818384, 11501858789, 11502818486, 11503778083, 11504788183, 11505818386, 11506828587, 11507828587, 11508717476, 11509737679, 11510798284, 11511818385, 11512838688, 11513778082, 11514778083, 11515576063, 11516737679, 11517677073, 11518777982, 11519848689, 11520818386, 11521677073, 11522737679, 11523778082, 11524727578, 11525586165, 11526495356, 11527788083, 9201828587, 9202828588, 9203808386, 9204767982, 9205808386, 9206858890, 9207828588, 9208667073, 9209677174, 9210788284, 9211747780, 9212808386, 9213778083, 9214757982, 9215566064, 9216687174, 9217677074, 9218778083, 9219848789, 9220858890, 9221667073, 9222828587, 9223808386, 9224747781, 9225576164, 9226465054, 9227778083, 2501828485, 2502727375, 2503767879, 2504757778, 2505777880, 2506818384, 2507788081, 2508666870, 2509646668, 2510717374, 2511676870, 2512788081, 2513707274, 2514697072, 2515626365, 2516687072, 2517707274, 2518838586, 2519889091, 2520868889, 2521626466, 2522798182, 2523798082, 2524707273, 2525575961, 2526535456, 2527777980, 12201909293, 12202848687, 12203848587, 12204828485, 12205858687, 12206848587, 12207848587, 12208757678, 12209747678, 12210777880, 12211787981, 12212838486, 12213818384, 12214788082, 12215697173, 12216777981, 12217767879, 12218848687, 12219868788, 12220878990, 12221697173, 12222868889, 12223868789, 12224788081, 12225676870, 12226616365, 12227838586, 2401859093, 2402818689, 2403838891, 2404818689, 2405858992, 2406858993, 2407808589, 2408747983, 2409768185, 2410788387, 2411818689, 2412808588, 2413778286, 2414768185, 2415626873, 2416869093, 2417707580, 2418667277, 2419677277, 2420697580, 2421697579, 2422798488, 2423818689, 2424758085, 2425546065, 2426525965, 2427808488, 12301899192, 12302858789, 12303858789, 12304828486, 12305838586, 12306838587, 12307828486, 12308677072, 12309687173, 12310666971, 12311717476, 12312858789, 12313788082, 12314747678, 12315677072, 12316778082, 12317737678, 12318848688, 12319909193, 12320889091, 12321727476, 12322868789, 12323868889, 12324747678, 12325555861, 12326596265, 12327868889, 12401899092, 12402808183, 12403889091, 12404788082, 12405868789, 12406808284, 12407737577, 12408676971, 12409737577, 12410737577, 12411707274, 12412868889, 12413717375, 12414676971, 12415727476, 12416788082, 12417788081, 12418798183, 12419868789, 12420868889, 12421646668, 12422737577, 12423808283, 12424727476, 12425535557, 12426272931, 12427868789, 9301899192, 9302838587, 9303848688, 9304798183, 9305838587, 9306848688, 9307808284, 9308697173, 9309697274, 9310717376, 9311717375, 9312858788, 9313777981, 9314737577, 9315697173, 9316777981, 9317747678, 9318838587, 9319899091, 9320868890, 9321687073, 9322828485, 9323858688, 9324757879, 9325545659, 9326545659, 9327858688, 12501818486, 12502788082, 12503798284, 12504788083, 12505798284, 12506838587, 12507798284, 12508737678, 12509687173, 12510676972, 12511747679, 12512808385, 12513737678, 12514717376, 12515606365, 12516747779, 12517697274, 12518767880, 12519838587, 12520808284, 12521687073, 12522838587, 12523818385, 12524727577, 12525636669, 12526535659, 12527757880, 2601848687, 2602777981, 2603757880, 2604767880, 2605808284, 2606818384, 2607808284, 2608727477, 2609666971, 2610717375, 2611697173, 2612788082, 2613767880, 2614747679, 2615535558, 2616777981, 2617697173, 2618878990, 2619919293, 2620899091, 2621666971, 2622858789, 2623818384, 2624717375, 2625575961, 2626596164, 2627767880, 12601868788, 12602808283, 12603878889, 12604747677, 12605818284, 12606787981, 12607707173, 12608586062, 12609636567, 12610505254, 12611586062, 12612818384, 12613656769, 12614616365, 12615606263, 12616808283, 12617717375, 12618818284, 12619878990, 12620878890, 12621586062, 12622777980, 12623848586, 12624656769, 12625434547, 12626384042, 12627767879, 2701888991, 2702838587, 2703828486, 2704798183, 2705828486, 2706858788, 2707818385, 2708767981, 2709767880, 2710697173, 2711737577, 2712858789, 2713798183, 2714737577, 2715676971, 2716757779, 2717757779, 2718899092, 2719919394, 2720909293, 2721666871, 2722878890, 2723889091, 2724777981, 2725565961, 2726535558, 2727848687, 9401878991, 9402767981, 9403838587, 9404777981, 9405808284, 9406838486, 9407788082, 9408717375, 9409687173, 9410676971, 9411697274, 9412848688, 9413767880, 9414717375, 9415666971, 9416767880, 9417757779, 9418858788, 9419868889, 9420868890, 9421687072, 9422798183, 9423838586, 9424727476, 9425555760, 9426616466, 9427808284, 11401728086, 11402616976, 11403717985, 11404677581, 11405707884, 11406778489, 11407748287, 11408637279, 11409556371, 11410677582, 11411657380, 11412778489, 11413707884, 11414586775, 11415596875, 11416556472, 11417677582, 11418758388, 11419647279, 11420707884, 11421354352, 11422233140, 11423778489, 11424586774, 11425354452, 11426263443, 11427768389, 12701909192, 12702838485, 12703878890, 12704767879, 12705858687, 12706838586, 12707757678, 12708666870, 12709697173, 12710687072, 12711656769, 12712858788, 12713747577, 12714676970, 12715727475, 12716818284, 12717798182, 12718808183, 12719858788, 12720888991, 12721666870, 12722838485, 12723899091, 12724777980, 12725586062, 12726485052, 12727848687, 2801868890, 2802838688, 2803788183, 2804767981, 2805838587, 2806828587, 2807838587, 2808687174, 2809707275, 2810818486, 2811788183, 2812858789, 2813767981, 2814777982, 2815616466, 2816697275, 2817717476, 2818788183, 2819899192, 2820868890, 2821717477, 2822798284, 2823818486, 2824778082, 2825646769, 2826495255, 2827798183, 12801909193, 12802848687, 12803899092, 12804808183, 12805838586, 12806828385, 12807767879, 12808676971, 12809717375, 12810575961, 12811676971, 12812868789, 12813747677, 12814666870, 12815687072, 12816828485, 12817788081, 12818848587, 12819888990, 12820878889, 12821656769, 12822838486, 12823878889, 12824757678, 12825555759, 12826525557, 12827868788, 2901888991, 2902808283, 2903838486, 2904828485, 2905828385, 2906868789, 2907828486, 2908747678, 2909737577, 2910737577, 2911676971, 2912858688, 2913777981, 2914727476, 2915646668, 2916727476, 2917777980, 2918868789, 2919899091, 2920878990, 2921636567, 2922899092, 2923899091, 2924747778, 2925586062, 2926545659, 2927838587, 3001697580, 3002737983, 3003677378, 3004616773, 3005707681, 3006677479, 3007606672, 3008485461, 3009596571, 3010596672, 3011636975, 3012677378, 3013536066, 3014556268, 3015263238, 3016505763, 3017424955, 3018535966, 3019778287, 3020626874, 3021566268, 3022667277, 3023556167, 3024515764, 3025374349, 3026354248, 3027515864, 12901878990, 12902828587, 12903798183, 12904767881, 12905828486, 12906828486, 12907828486, 12908666871, 12909676972, 12910838587, 12911737678, 12912848688, 12913798284, 12914777981, 12915636668, 12916737678, 12917676972, 12918818385, 12919838687, 12920868889, 12921687173, 12922828587, 12923858789, 12924747779, 12925616467, 12926525558, 12927788183, 3101808385, 3102757880, 3103788183, 3104626669, 3105767981, 3106788183, 3107626568, 3108586265, 3109606366, 3110576063, 3111656871, 3112737679, 3113596265, 3114545761, 3115424548, 3116687174, 3117626568, 3118697274, 3119808385, 3120757880, 3121454851, 3122636670, 3123717477, 3124626669, 3125363943, 3126464953, 3127646770, 13001848688, 13002767880, 13003757779, 13004757779, 13005788082, 13006818385, 13007777981, 13008727476, 13009687173, 13010737577, 13011727577, 13012828486, 13013747779, 13014717476, 13015616366, 13016717375, 13017717476, 13018848688, 13019868889, 13020848688, 13021636668, 13022858788, 13023848688, 13024747678, 13025596164, 13026545659, 13027777981, 3201939597, 3202838790, 3203828689, 3204788386, 3205838790, 3206879193, 3207828690, 3208727781, 3209727781, 3210697478, 3211697478, 3212909395, 3213868992, 3214768084, 3215747882, 3216657075, 3217737882, 3218818588, 3219848891, 3220858992, 3221828689, 3222909395, 3223818589, 3224778185, 3225626771, 3226455055, 3227879093, 3301929597, 3302869193, 3303869093, 3304808589, 3305838891, 3306858992, 3307808488, 3308747983, 3309808589, 3310808588, 3311788287, 3312889295, 3313848992, 3314838791, 3315758084, 3316687479, 3317788387, 3318818690, 3319838891, 3320848891, 3321748084, 3322869093, 3323858992, 3324838791, 3325697579, 3326535964, 3327869093, 3401858789, 3402777982, 3403828587, 3404778082, 3405808285, 3406858789, 3407747779, 3408646870, 3409737678, 3410545760, 3411596265, 3412848688, 3413727578, 3414697275, 3415626568, 3416778083, 3417757881, 3418818386, 3419828486, 3420868991, 3421646770, 3422858789, 3423868890, 3424707376, 3425545760, 3426535659, 3427828487, 13101868889, 13102787981, 13103808183, 13104808183, 13105808283, 13106818284, 13107828385, 13108727476, 13109697173, 13110707274, 13111717375, 13112858687, 13113767880, 13114737577, 13115687071, 13116697173, 13117747577, 13118838586, 13119889091, 13120878889, 13121676971, 13122858688, 13123868789, 13124767779, 13125626466, 13126565860, 13127808284, 15701889092, 15702818487, 15703828588, 15704808386, 15705848789, 15706868991, 15707788284, 15708717578, 15709767982, 15710697376, 15711798285, 15712868891, 15713788184, 15714727679, 15715606468, 15716798285, 15717717578, 15718778184, 15719798285, 15720828588, 15721646872, 15722757882, 15723858890, 15724757982, 15725535761, 15726454953, 15727798285, 9501838486, 9502747678, 9503777981, 9504798082, 9505788082, 9506818385, 9507798182, 9508707274, 9509687072, 9510767880, 9511757778, 9512818385, 9513767880, 9514727476, 9515616365, 9516666870, 9517717374, 9518828485, 9519888990, 9520868788, 9521656769, 9522828385, 9523808283, 9524727475, 9525596264, 9526616365, 9527767880, 9601888991, 9602808284, 9603848687, 9604798183, 9605818385, 9606818385, 9607808284, 9608747678, 9609727476, 9610636668, 9611747678, 9612858788, 9613777981, 9614757779, 9615666870, 9616707274, 9617777981, 9618848587, 9619888991, 9620878990, 9621666871, 9622848687, 9623868889, 9624757779, 9625626466, 9626525557, 9627828486, 3501788489, 3502717884, 3503616976, 3504596774, 3505697783, 3506758288, 3507667481, 3508657379, 3509556371, 3510677581, 3511677582, 3512738086, 3513677582, 3514687682, 3515384654, 3516647279, 3517576573, 3518677481, 3519627077, 3520667380, 3521596774, 3522677581, 3523717884, 3524687582, 3525536169, 3526475664, 3527606875, 3601838587, 3602767880, 3603889091, 3604798183, 3605798183, 3606818385, 3607777981, 3608596264, 3609646669, 3610535658, 3611555860, 3612889091, 3613707375, 3614656870, 3615656770, 3616777981, 3617757779, 3618868890, 3619868889, 3620919394, 3621646769, 3622868889, 3623868789, 3624697274, 3625636668, 3626525457, 3627798183, 13201899192, 13202818385, 13203858789, 13204778082, 13205828486, 13206838687, 13207818385, 13208707375, 13209697174, 13210677073, 13211717476, 13212858788, 13213778082, 13214737578, 13215676972, 13216737578, 13217717476, 13218848688, 13219899092, 13220889092, 13221666971, 13222848688, 13223868890, 13224737578, 13225565962, 13226576063, 13227838587, 9701899091, 9702788081, 9703848587, 9704777980, 9705818284, 9706838485, 9707757779, 9708676870, 9709707274, 9710666870, 9711676970, 9712858788, 9713747677, 9714697173, 9715707274, 9716788081, 9717788082, 9718858788, 9719899091, 9720878889, 9721606264, 9722798182, 9723828485, 9724737576, 9725515355, 9726454749, 9727818384, 3701838586, 3702777880, 3703868788, 3704727475, 3705788081, 3706808183, 3707697173, 3708596163, 3709616264, 3710616264, 3711596163, 3712808283, 3713656668, 3714596163, 3715525456, 3716737576, 3717666769, 3718808183, 3719858788, 3720838586, 3721555759, 3722747577, 3723798182, 3724656769, 3725474951, 3726444648, 3727767879, 3801858788, 3802788082, 3803798183, 3804788082, 3805808284, 3806838486, 3807808284, 3808727476, 3809697174, 3810707375, 3811717375, 3812838587, 3813757779, 3814727476, 3815656870, 3816727576, 3817747678, 3818828385, 3819878890, 3820848587, 3821676972, 3822838586, 3823838587, 3824767879, 3825626567, 3826505355, 3827798183, 9801909192, 9802788082, 9803888991, 9804798183, 9805818385, 9806858688, 9807777981, 9808636568, 9809676971, 9810707274, 9811687072, 9812888991, 9813767879, 9814727476, 9815717375, 9816757779, 9817757779, 9818838586, 9819868789, 9820888991, 9821656870, 9822828486, 9823848687, 9824707274, 9825565860, 9826353840, 9827858788, 9901929597, 9902788388, 9903889295, 9904848992, 9905798488, 9906818690, 9907818690, 9908808690, 9909768286, 9910727883, 9911727883, 9912879194, 9913768286, 9914727883, 9915697681, 9916748085, 9917737984, 9918808589, 9919838892, 9920808589, 9921647176, 9922748085, 9923848992, 9924778387, 9925707681, 9926525966, 9927869194, 10001888990, 10002818384, 10003868889, 10004787981, 10005838586, 10006828385, 10007777879, 10008676970, 10009697173, 10010636566, 10011666869, 10012848586, 10013737576, 10014676971, 10015666769, 10016808283, 10017757678, 10018828384, 10019909192, 10020868788, 10021616365, 10022818283, 10023858687, 10024717374, 10025535556, 10026616364, 10027818284, 4001828690, 4002838791, 4003747984, 4004768185, 4005848891, 4006838791, 4007808488, 4008747983, 4009677277, 4010848891, 4011808588, 4012808588, 4013798487, 4014758084, 4015525863, 4016697479, 4017606671, 4018889294, 4019929597, 4020858992, 4021737882, 4022747983, 4023858992, 4024758084, 4025636974, 4026747983, 4027788387, 4101899091, 4102838485, 4103838486, 4104798182, 4105838586, 4106858687, 4107838586, 4108777880, 4109747678, 4110727476, 4111737576, 4112848586, 4113798082, 4114787981, 4115707274, 4116767779, 4117788081, 4118888990, 4119889091, 4120888990, 4121686971, 4122868788, 4123848687, 4124798182, 4125636567, 4126586062, 4127838586, 4201919597, 4202848992, 4203778387, 4204737984, 4205808589, 4206808589, 4207818690, 4208707681, 4209737984, 4210879194, 4211828791, 4212869194, 4213788488, 4214778387, 4215727883, 4216727883, 4217778287, 4218808690, 4219848993, 4220848992, 4221657177, 4222808589, 4223768286, 4224748085, 4225525865, 4226566369, 4227798489, 4301858991, 4302778185, 4303757983, 4304768084, 4305798387, 4306838790, 4307828689, 4308697377, 4309677276, 4310747882, 4311697378, 4312788285, 4313768083, 4314737881, 4315596469, 4316727781, 4317677276, 4318818588, 4319879093, 4320848790, 4321697377, 4322848890, 4323858891, 4324768083, 4325606469, 4326566165, 4327778185, 10101888990, 10102767880, 10103838587, 10104767880, 10105798082, 10106798183, 10107757779, 10108676971, 10109687072, 10110616366, 10111656769, 10112858788, 10113757779, 10114707274, 10115676971, 10116727476, 10117747577, 10118848587, 10119878990, 10120868889, 10121626466, 10122808284, 10123848687, 10124737577, 10125626466, 10126545759, 10127818384, 13301899192, 13302848687, 13303858688, 13304828485, 13305858788, 13306858788, 13307838586, 13308717375, 13309727375, 13310717375, 13311737476, 13312878890, 13313818384, 13314777981, 13315676971, 13316808283, 13317747678, 13318838486, 13319899192, 13320909192, 13321747678, 13322878990, 13323858688, 13324777981, 13325646668, 13326565860, 13327868789, 10201899091, 10202818284, 10203868889, 10204788081, 10205828485, 10206858687, 10207777980, 10208717374, 10209717274, 10210727375, 10211717274, 10212858688, 10213777880, 10214727475, 10215727375, 10216798081, 10217808183, 10218858688, 10219899091, 10220909192, 10221686971, 10222828385, 10223868788, 10224777880, 10225656768, 10226596163, 10227838485, 4401909394, 4402818486, 4403808386, 4404788184, 4405838588, 4406838689, 4407808385, 4408747780, 4409767982, 4410778083, 4411798284, 4412848689, 4413798285, 4414778083, 4415626669, 4416767982, 4417747780, 4418818486, 4419858890, 4420868890, 4421717578, 4422848789, 4423808386, 4424767982, 4425626669, 4426555962, 4427818487, 8001849094, 8002849093, 8003788589, 8004758288, 8005788589, 8006828893, 8007879396, 8008606875, 8009657379, 8010536169, 8011778389, 8012889396, 8013768388, 8014758288, 8015546269, 8016687682, 8017667480, 8018778489, 8019828893, 8020838993, 8021758288, 8022869295, 8023697682, 8024677581, 8025536168, 8026435160, 8027768388, 4501878889, 4502798082, 4503808182, 4504798081, 4505818283, 4506838485, 4507808283, 4508727475, 4509707273, 4510747677, 4511707173, 4512848687, 4513798082, 4514767779, 4515707274, 4516808182, 4517787981, 4518878889, 4519899091, 4520899091, 4521666869, 4522858688, 4523848687, 4524757778, 4525596163, 4526515355, 4527828485, 11301848586, 11302777880, 11303868788, 11304767779, 11305828385, 11306838486, 11307717374, 11308636567, 11309697072, 11310596163, 11311626466, 11312848687, 11313697072, 11314656769, 11315616364, 11316788081, 11317767879, 11318818284, 11319878990, 11320899091, 11321616365, 11322777880, 11323848687, 11324727375, 11325565860, 11326495153, 11327798182, 4601868890, 4602798184, 4603808284, 4604788183, 4605818386, 4606858890, 4607798284, 4608757880, 4609707376, 4610727578, 4611778082, 4612818486, 4613767881, 4614737678, 4615666972, 4616788183, 4617757780, 4618788183, 4619858789, 4620828587, 4621687174, 4622828486, 4623838587, 4624757881, 4625697274, 4626596265, 4627778082, 8101858789, 8102788184, 8103878991, 8104777982, 8105808285, 8106798184, 8107646771, 8108596266, 8109707376, 8110757780, 8111707376, 8112868890, 8113697275, 8114656872, 8115798284, 8116788083, 8117828587, 8118798284, 8119838588, 8120868991, 8121475053, 8122606366, 8123848688, 8124677073, 8125485154, 8126454852, 8127818486, 4701828486, 4702767880, 4703777981, 4704777981, 4705808284, 4706818385, 4707808284, 4708707274, 4709687072, 4710707275, 4711697274, 4712808284, 4713747679, 4714737577, 4715596264, 4716747678, 4717697174, 4718767880, 4719838587, 4720828486, 4721646669, 4722848587, 4723818385, 4724737577, 4725606365, 4726565861, 4727778082, 4801828994, 4802738288, 4803758389, 4804748389, 4805738288, 4806798692, 4807738187, 4808687784, 4809738288, 4810667582, 4811748389, 4812849195, 4813718086, 4814728187, 4815617078, 4816748288, 4817677683, 4818717986, 4819667583, 4820748288, 4821627279, 4822788691, 4823748288, 4824647381, 4825536271, 4826465565, 4827758389, 4901899192, 4902828385, 4903868889, 4904818385, 4905878889, 4906878990, 4907808283, 4908747678, 4909737577, 4910717375, 4911676971, 4912909293, 4913818284, 4914777880, 4915798082, 4916848687, 4917838586, 4918919293, 4919909293, 4920919293, 4921727476, 4922899091, 4923929394, 4924788081, 4925697173, 4926666870, 4927878990, 3901828891, 3902818690, 3903768286, 3904788388, 3905848992, 3906838892, 3907828791, 3908798589, 3909748085, 3910808589, 3911838892, 3912808589, 3913778287, 3914788488, 3915606672, 3916788388, 3917697581, 3918465360, 3919606773, 3920697581, 3921727883, 3922869194, 3923818690, 3924727883, 3925667278, 3926566369, 3927778387, 10301878889, 10302777980, 10303858687, 10304757677, 10305818384, 10306828485, 10307757778, 10308656668, 10309676970, 10310666769, 10311656768, 10312848687, 10313737476, 10314676970, 10315666769, 10316787980, 10317757678, 10318818384, 10319878889, 10320878889, 10321616264, 10322808283, 10323858687, 10324717274, 10325525355, 10326434546, 10327808182, 5001888990, 5002787981, 5003808182, 5004798081, 5005828384, 5006838485, 5007788081, 5008737475, 5009717274, 5010798081, 5011737476, 5012858687, 5013798081, 5014757778, 5015707173, 5016757677, 5017777879, 5018818283, 5019888990, 5020868788, 5021646567, 5022818283, 5023848586, 5024787980, 5025646566, 5026585961, 5027828384, 5201838486, 5202747677, 5203757678, 5204767879, 5205767879, 5206788082, 5207788081, 5208707274, 5209676970, 5210676971, 5211727476, 5212798183, 5213747678, 5214707274, 5215626466, 5216697173, 5217747678, 5218757779, 5219848687, 5220818384, 5221646668, 5222798082, 5223777980, 5224717375, 5225616365, 5226576062, 5227757779, 13401888990, 13402788081, 13403858687, 13404767779, 13405818284, 13406838485, 13407767879, 13408626465, 13409666870, 13410616364, 13411616365, 13412848586, 13413737476, 13414707173, 13415646668, 13416777980, 13417737576, 13418848587, 13419888991, 13420878890, 13421626365, 13422848687, 13423848687, 13424727375, 13425596062, 13426505254, 13427818283, 5301889194, 5302838790, 5303848891, 5304788387, 5305858992, 5306858992, 5307808588, 5308707580, 5309737882, 5310798387, 5311818589, 5312848891, 5313768185, 5314758084, 5315717681, 5316768185, 5317788386, 5318788387, 5319879194, 5320828690, 5321697478, 5322869093, 5323858992, 5324788286, 5325657075, 5326586368, 5327838790, 13501838587, 13502777981, 13503808284, 13504798183, 13505818385, 13506838587, 13507828486, 13508717476, 13509697274, 13510757779, 13511757880, 13512838587, 13513767981, 13514727477, 13515636668, 13516747779, 13517717376, 13518677072, 13519848688, 13520798284, 13521646769, 13522828486, 13523808284, 13524717476, 13525606365, 13526505356, 13527777982, 13601868789, 13602798082, 13603798082, 13604777980, 13605828384, 13606828485, 13607818283, 13608717274, 13609717374, 13610777980, 13611697173, 13612838586, 13613767879, 13614747577, 13615626466, 13616697072, 13617727475, 13618848687, 13619899091, 13620878990, 13621646567, 13622858687, 13623848586, 13624757779, 13625626466, 13626565860, 13627808182, 5401828689, 5402798387, 5403717680, 5404778185, 5405828689, 5406778185, 5407788285, 5408687277, 5409667075, 5410727680, 5411737882, 5412858891, 5413798386, 5414757983, 5415535863, 5416677276, 5417646974, 5418788286, 5419889194, 5420838790, 5421687277, 5422788286, 5423757983, 5424747882, 5425677176, 5426616670, 5427768184, 5501909192, 5502808182, 5503828384, 5504808283, 5505838485, 5506858687, 5507828384, 5508777879, 5509747677, 5510818283, 5511777880, 5512878889, 5513808182, 5514777879, 5515767778, 5516747677, 5517777980, 5518878889, 5519909192, 5520899091, 5521717273, 5522868788, 5523838586, 5524777980, 5525626465, 5526636566, 5527868788, 13701888990, 13702808182, 13703868788, 13704777980, 13705838485, 13706828384, 13707767779, 13708656768, 13709676970, 13710646567, 13711626466, 13712858687, 13713737476, 13714687071, 13715646667, 13716727475, 13717737476, 13718808183, 13719899091, 13720878889, 13721646567, 13722818284, 13723818283, 13724697172, 13725515254, 13726505254, 13727818283, 8201868787, 8202818182, 8203909191, 8204858586, 8205878788, 8206838484, 8207797980, 8208808181, 8209848485, 8210878788, 8211899090, 8212888889, 8213818182, 8214757677, 8215828283, 8216787980, 8217828283, 8218848585, 8219838484, 8220878788, 8221535454, 8222585960, 8223737475, 8224656666, 8225404142, 8226454647, 8227878888, 8501838790, 8502788286, 8503838790, 8504747882, 8505758084, 8506818588, 8507646974, 8508667176, 8509717680, 8510505561, 8511646973, 8512788286, 8513626771, 8514576267, 8515515661, 8516687377, 8517697479, 8518737882, 8519879093, 8520828689, 8521556065, 8522576267, 8523758084, 8524737882, 8525475257, 8526485358, 8527737781, 5601858688, 5602777981, 5603777981, 5604757779, 5605808283, 5606818284, 5607767880, 5608676972, 5609656870, 5610626466, 5611666871, 5612838586, 5613727476, 5614697174, 5615656769, 5616777980, 5617727476, 5618777981, 5619868789, 5620848688, 5621666870, 5622858688, 5623828486, 5624737577, 5625586062, 5626424447, 5627798183, 5701727984, 5702626975, 5703677480, 5704586572, 5705636975, 5706687480, 5707626975, 5708596672, 5709435057, 5710546167, 5711536067, 5712758186, 5713576471, 5714505764, 5715394653, 5716667379, 5717515865, 5718505764, 5719465461, 5720546168, 5721424956, 5722737984, 5723687480, 5724576470, 5725435057, 5726334047, 5727556268, 13801899092, 13802848587, 13803878889, 13804828385, 13805858687, 13806848587, 13807848587, 13808757778, 13809727476, 13810777980, 13811747678, 13812868789, 13813798182, 13814777880, 13815666870, 13816777880, 13817767879, 13818848587, 13819868889, 13820868789, 13821697173, 13822888991, 13823878889, 13824767879, 13825616365, 13826626466, 13827838586, 5801788387, 5802768286, 5803717681, 5804747984, 5805808589, 5806778286, 5807798488, 5808657176, 5809677378, 5810808589, 5811717681, 5812778286, 5813677378, 5814717781, 5815495662, 5816707681, 5817616773, 5818737984, 5819798488, 5820808589, 5821657176, 5822768186, 5823808589, 5824657176, 5825576369, 5826546066, 5827717681, 13901909192, 13902838586, 13903838485, 13904798182, 13905828385, 13906838485, 13907808283, 13908727375, 13909717375, 13910737577, 13911737576, 13912878889, 13913808183, 13914757778, 13915676971, 13916747577, 13917737577, 13918878889, 13919909192, 13920888990, 13921676970, 13922848687, 13923878990, 13924777880, 13925626466, 13926525456, 13927858687, 5901727781, 5902667075, 5903697478, 5904707579, 5905697378, 5906747882, 5907636872, 5908626771, 5909606569, 5910576266, 5911616671, 5912687377, 5913657075, 5914646973, 5915586368, 5916768184, 5917697478, 5918697478, 5919798387, 5920747983, 5921606569, 5922818588, 5923727680, 5924576267, 5925545964, 5926505560, 5927677176, 6001879092, 6002838789, 6003818588, 6004778184, 6005818588, 6006838689, 6007747881, 6008737780, 6009717579, 6010616670, 6011677175, 6012848890, 6013747881, 6014727680, 6015586266, 6016697377, 6017707478, 6018626671, 6019798386, 6020828588, 6021667074, 6022848790, 6023818588, 6024788285, 6025566165, 6026535762, 6027798386, 6101848587, 6102747577, 6103848587, 6104747678, 6105767880, 6106788082, 6107697173, 6108636567, 6109646668, 6110545658, 6111575961, 6112818384, 6113697173, 6114626466, 6115636668, 6116737576, 6117747577, 6118788081, 6119798183, 6120828485, 6121586062, 6122788082, 6123798183, 6124697173, 6125525456, 6126545658, 6127798082, 6201868789, 6202787981, 6203858688, 6204787981, 6205798082, 6206818384, 6207777880, 6208666870, 6209666870, 6210596163, 6211636567, 6212858687, 6213717375, 6214646668, 6215656769, 6216767779, 6217757779, 6218848687, 6219919293, 6220878889, 6221616365, 6222838486, 6223818384, 6224666870, 6225545658, 6226505355, 6227798183, 6301737781, 6302697478, 6303636772, 6304646973, 6305727680, 6306697478, 6307757983, 6308545963, 6309626771, 6310636872, 6311677175, 6312747882, 6313677176, 6314646873, 6315444954, 6316586367, 6317596368, 6318636872, 6319737781, 6320757983, 6321505559, 6322737881, 6323677175, 6324606469, 6325535863, 6326394449, 6327636772, 10401889091, 10402798183, 10403838586, 10404777980, 10405818385, 10406838586, 10407777980, 10408687072, 10409687072, 10410646668, 10411687072, 10412848687, 10413747678, 10414697173, 10415707274, 10416808284, 10417777981, 10418757779, 10419798082, 10420838486, 10421666870, 10422848587, 10423828486, 10424717375, 10425525456, 10426545759, 10427828485, 6401838892, 6402727984, 6403647177, 6404717883, 6405758186, 6406768287, 6407717783, 6408677479, 6409626975, 6410687480, 6411707682, 6412818791, 6413748186, 6414667278, 6415677479, 6416616874, 6417707682, 6418717883, 6419798589, 6420778387, 6421667379, 6422849093, 6423788489, 6424748085, 6425586572, 6426515966, 6427758186, 6501919394, 6502848688, 6503848688, 6504858788, 6505858788, 6506868890, 6507858789, 6508757780, 6509767981, 6510727477, 6511767881, 6512878991, 6513838587, 6514798183, 6515757880, 6516757780, 6517777982, 6518838687, 6519878991, 6520909293, 6521697174, 6522868889, 6523878991, 6524788083, 6525666971, 6526586164, 6527889092, 6601878991, 6602808284, 6603788183, 6604747780, 6605818486, 6606828587, 6607757880, 6608697275, 6609656871, 6610747779, 6611717477, 6612838587, 6613757880, 6614707376, 6615677073, 6616727577, 6617747679, 6618858789, 6619909293, 6620878991, 6621606466, 6622808285, 6623828486, 6624747780, 6625586265, 6626606366, 6627808285, 8301838892, 8302798589, 8303707681, 8304667378, 8305737984, 8306768186, 8307737984, 8308657277, 8309697580, 8310657177, 8311657177, 8312828791, 8313758186, 8314707782, 8315576470, 8316596672, 8317677379, 8318707681, 8319778387, 8320778287, 8321768186, 8322838892, 8323768286, 8324727883, 8325576470, 8326525965, 8327788388, 6701879296, 6702899497, 6703778489, 6704738086, 6705828993, 6706808792, 6707849094, 6708606875, 6709667480, 6710687582, 6711828893, 6712838993, 6713818892, 6714808792, 6715485765, 6716677581, 6717647279, 6718859195, 6719838993, 6720838993, 6721778489, 6722838993, 6723768388, 6724647279, 6725576573, 6726455361, 6727798691, 6801899192, 6802848687, 6803858789, 6804788082, 6805828486, 6806848687, 6807767880, 6808677072, 6809687173, 6810676972, 6811707275, 6812858789, 6813777981, 6814717375, 6815737578, 6816798183, 6817747678, 6818808284, 6819858788, 6820858789, 6821676972, 6822818385, 6823858789, 6824757779, 6825565962, 6826555861, 6827858789, 8401949798, 8402879194, 8403939698, 8404768185, 8405828690, 8406859093, 8407848892, 8408576368, 8409667176, 8410596571, 8411606671, 8412808488, 8413636974, 8414616772, 8415495561, 8416657176, 8417566268, 8418798488, 8419889295, 8420858992, 8421677278, 8422858992, 8423909396, 8424566268, 8425394552, 8426717782, 8427838891, 6901889396, 6902808690, 6903808690, 6904788589, 6905818691, 6906828792, 6907838993, 6908778388, 6909768388, 6910697682, 6911838892, 6912818791, 6913778388, 6914808690, 6915616874, 6916768388, 6917717883, 6918737985, 6919657278, 6920778388, 6921677480, 6922828792, 6923848993, 6924738085, 6925727984, 6926576572, 6927758186, 14001858688, 14002788081, 14003777880, 14004777981, 14005808283, 14006818384, 14007818384, 14008737577, 14009697173, 14010777880, 14011737577, 14012838486, 14013757779, 14014757678, 14015616364, 14016747678, 14017717375, 14018848687, 14019899091, 14020878890, 14021676971, 14022848687, 14023808183, 14024737576, 14025586062, 14026555759, 14027777981, 10501909192, 10502818384, 10503878890, 10504798082, 10505848586, 10506848687, 10507777980, 10508666870, 10509737576, 10510666870, 10511697173, 10512868789, 10513767880, 10514727475, 10515656769, 10516798182, 10517767779, 10518868889, 10519899091, 10520909192, 10521687071, 10522858788, 10523858687, 10524767879, 10525616365, 10526727475, 10527848587, 7001888990, 7002838485, 7003828485, 7004818283, 7005838586, 7006858687, 7007858687, 7008717374, 7009717274, 7010697172, 7011717374, 7012828384, 7013798081, 7014757678, 7015646567, 7016656769, 7017707173, 7018848586, 7019909192, 7020888990, 7021697072, 7022868789, 7023878889, 7024767879, 7025596162, 7026495153, 7027818284, 7101899092, 7102838587, 7103798183, 7104798183, 7105828486, 7106848688, 7107838587, 7108737577, 7109727577, 7110707275, 7111808284, 7112888991, 7113828485, 7114788183, 7115666971, 7116777981, 7117757880, 7118848688, 7119899092, 7120878890, 7121717476, 7122868890, 7123868889, 7124818385, 7125666971, 7126485053, 7127818385, 14101888990, 14102818284, 14103808284, 14104808284, 14105818384, 14106838486, 14107838587, 14108737577, 14109757779, 14110757779, 14111777981, 14112858788, 14113808283, 14114788082, 14115626466, 14116747678, 14117737577, 14118818385, 14119858688, 14120848687, 14121707274, 14122868889, 14123858688, 14124777980, 14125676971, 14126545658, 14127808183, 14201888991, 14202818384, 14203868889, 14204808183, 14205828485, 14206818384, 14207777980, 14208697173, 14209707274, 14210687072, 14211676971, 14212868788, 14213757778, 14214707274, 14215707273, 14216798082, 14217777980, 14218838586, 14219868889, 14220878990, 14221666870, 14222828485, 14223868788, 14224747577, 14225616365, 14226505254, 14227838586, 14301949596, 14302909293, 14303939496, 14304858789, 14305889092, 14306868890, 14307828587, 14308777982, 14309818486, 14310717476, 14311808285, 14312949697, 14313838688, 14314788083, 14315818486, 14316889092, 14317858890, 14318868890, 14319868890, 14320919394, 14321808285, 14322838587, 14323929495, 14324818386, 14325626669, 14326666972, 14327919394, 14401838891, 14402838891, 14403768185, 14404798488, 14405798488, 14406818689, 14407818589, 14408697579, 14409758084, 14410707580, 14411778286, 14412818589, 14413768185, 14414758084, 14415646975, 14416768186, 14417707681, 14418788387, 14419768186, 14420808488, 14421707680, 14422858992, 14423848891, 14424808589, 14425707681, 14426576369, 14427758084, 7201818690, 7202717782, 7203768186, 7204778287, 7205838791, 7206818690, 7207818690, 7208586470, 7209566268, 7210576470, 7211586571, 7212859093, 7213768286, 7214687480, 7215707681, 7216727883, 7217717782, 7218859093, 7219899396, 7220869194, 7221828791, 7222909496, 7223748085, 7224616773, 7225475460, 7226758085, 7227768286, 7301909294, 7302858890, 7303808386, 7304808486, 7305828588, 7306808386, 7307808386, 7308707477, 7309697376, 7310697376, 7311737679, 7312858891, 7313808386, 7314768083, 7315687275, 7316757982, 7317747881, 7318798285, 7319828688, 7320848790, 7321697276, 7322858890, 7323858890, 7324778184, 7325646872, 7326535862, 7327838689, 7401798386, 7402798285, 7403868992, 7404717579, 7405717579, 7406757982, 7407808487, 7408505459, 7409495357, 7410556064, 7411495358, 7412747881, 7413656973, 7414606468, 7415444953, 7416656973, 7417626670, 7418757882, 7419818588, 7420767983, 7421677174, 7422828688, 7423818487, 7424626771, 7425576165, 7426535862, 7427747881, 7501899192, 7502838587, 7503848688, 7504858788, 7505838587, 7506838587, 7507858789, 7508778082, 7509757880, 7510777981, 7511798183, 7512848688, 7513808284, 7514788082, 7515687072, 7516798284, 7517737577, 7518848688, 7519838587, 7520858789, 7521676972, 7522868789, 7523848687, 7524777981, 7525656870, 7526545659, 7527838587, 14501899193, 14502838688, 14503858890, 14504828487, 14505868890, 14506848789, 14507788183, 14508767982, 14509757880, 14510727578, 14511737679, 14512848688, 14513767982, 14514697376, 14515636770, 14516778083, 14517757881, 14518818386, 14519828587, 14520848789, 14521616467, 14522868991, 14523889092, 14524717577, 14525535760, 14526545760, 14527848789, 7601899396, 7602889395, 7603818791, 7604808590, 7605808690, 7606869194, 7607929598, 7608808690, 7609687581, 7610768287, 7611758186, 7612909597, 7613838892, 7614798589, 7615748085, 7616748085, 7617788488, 7618798589, 7619909497, 7620879295, 7621697581, 7622899496, 7623899396, 7624808690, 7625768287, 7626707682, 7627869194, 14601909192, 14602818385, 14603878890, 14604838486, 14605818384, 14606828486, 14607788081, 14608687072, 14609687072, 14610596163, 14611636567, 14612878990, 14613747678, 14614697173, 14615656769, 14616798183, 14617767880, 14618848687, 14619889091, 14620878990, 14621646668, 14622888990, 14623878990, 14624737576, 14625545658, 14626606264, 14627858688, 14701818588, 14702778083, 14703788184, 14704747881, 14705798285, 14706808486, 14707808386, 14708687276, 14709626770, 14710737780, 14711697376, 14712788184, 14713727679, 14714707478, 14715515660, 14716778184, 14717616569, 14718798285, 14719858891, 14720818487, 14721626770, 14722818487, 14723818487, 14724667073, 14725535862, 14726545963, 14727727679, 14801868890, 14802818385, 14803808285, 14804808284, 14805808385, 14806838587, 14807838587, 14808737578, 14809707375, 14810697174, 14811717376, 14812818386, 14813778082, 14814757780, 14815646669, 14816727578, 14817727578, 14818808284, 14819828486, 14820838587, 14821707275, 14822848688, 14823838587, 14824788082, 14825666972, 14826545760, 14827808284, 10601858688, 10602767880, 10603828385, 10604737577, 10605777980, 10606798182, 10607757778, 10608596163, 10609586163, 10610676971, 10611606265, 10612878889, 10613737576, 10614687072, 10615676971, 10616767879, 10617767880, 10618838586, 10619868889, 10620868889, 10621646668, 10622838486, 10623818385, 10624717375, 10625575961, 10626555860, 10627777981, 14901889091, 14902798183, 14903848587, 14904747678, 14905808284, 14906828486, 14907717375, 14908666870, 14909697173, 14910606264, 14911636568, 14912838586, 14913707274, 14914676971, 14915646769, 14916687072, 14917747678, 14918798183, 14919848687, 14920868789, 14921565861, 14922757779, 14923808183, 14924707274, 14925474952, 14926505254, 14927798183, 7701909293, 7702808283, 7703858687, 7704808283, 7705828385, 7706828485, 7707808283, 7708717374, 7709717274, 7710717374, 7711717374, 7712858788, 7713808183, 7714757678, 7715747678, 7716788081, 7717788081, 7718848587, 7719858687, 7720868788, 7721697172, 7722848586, 7723889091, 7724767879, 7725636567, 7726555759, 7727888990, 7801899192, 7802858688, 7803818385, 7804838587, 7805828485, 7806848687, 7807838586, 7808747678, 7809757779, 7810777980, 7811767880, 7812808284, 7813788082, 7814767880, 7815586063, 7816737577, 7817697274, 7818868889, 7819909192, 7820878990, 7821717375, 7822889091, 7823848687, 7824757779, 7825626467, 7826616365, 7827808283, 11001808892, 11002788591, 11003707885, 11004687784, 11005738187, 11006728087, 11007859195, 11008455463, 11009404958, 11010536271, 11011647380, 11012808892, 11013728087, 11014657481, 11015374655, 11016596876, 11017566573, 11018707885, 11019859296, 11020788591, 11021657481, 11022839094, 11023657481, 11024475665, 11025303948, 11026435362, 11027738187, 11101798387, 11102747883, 11103616671, 11104636873, 11105727781, 11106737882, 11107727781, 11108556065, 11109566267, 11110788387, 11111667276, 11112808488, 11113707579, 11114687378, 11115576267, 11116687478, 11117616671, 11118616771, 11119727681, 11120747983, 11121515762, 11122798487, 11123747983, 11124677277, 11125455056, 11126384349, 11127636873, 15001848687, 15002777880, 15003818384, 15004818284, 15005818384, 15006828485, 15007828385, 15008727476, 15009737476, 15010666870, 15011717374, 15012848587, 15013798082, 15014757678, 15015687072, 15016808283, 15017767880, 15018858688, 15019888990, 15020888991, 15021666869, 15022878990, 15023818384, 15024727476, 15025575961, 15026555759, 15027828385, 15101879091, 15102838688, 15103838688, 15104848688, 15105868890, 15106889092, 15107848688, 15108767981, 15109798184, 15110798184, 15111798284, 15112848789, 15113818486, 15114798184, 15115646770, 15116788183, 15117737679, 15118788183, 15119828487, 15120828587, 15121767982, 15122868890, 15123818486, 15124798284, 15125636770, 15126535660, 15127818386, 10701899091, 10702798082, 10703888990, 10704777980, 10705828485, 10706838586, 10707727475, 10708676870, 10709697172, 10710737576, 10711687072, 10712878889, 10713757678, 10714687072, 10715737476, 10716777980, 10717777980, 10718838485, 10719868889, 10720899091, 10721636567, 10722787981, 10723858687, 10724757779, 10725626466, 10726424446, 10727848586, 16001637077, 16002546169, 16003616875, 16004536168, 16005536168, 16006637077, 16007596774, 16008455260, 16009465361, 16010576572, 16011526067, 16012738086, 16013526068, 16014495765, 16015253240, 16016586572, 16017455361, 16018717884, 16019677480, 16020687581, 16021445260, 16022738086, 16023546269, 16024424957, 16025273442, 16026303847, 16027485664, 10801919394, 10802868890, 10803868889, 10804868890, 10805878990, 10806889091, 10807878990, 10808788082, 10809757880, 10810778082, 10811788183, 10812888991, 10813848688, 10814858789, 10815727476, 10816828486, 10817788183, 10818888991, 10819929394, 10820889092, 10821777981, 10822878990, 10823909193, 10824868889, 10825777981, 10826818385, 10827848688, 15801929393, 15802858788, 15803858687, 15804828385, 15805858687, 15806858688, 15807868788, 15808757678, 15809747677, 15810757778, 15811767779, 15812888990, 15813838486, 15814818283, 15815757677, 15816808183, 15817828485, 15818878990, 15819919292, 15820899091, 15821747577, 15822888990, 15823868789, 15824818384, 15825676970, 15826575960, 15827878889, 15901889091, 15902838587, 15903838587, 15904838587, 15905868789, 15906868789, 15907838587, 15908747678, 15909727476, 15910687173, 15911717375, 15912848687, 15913788082, 15914737577, 15915636567, 15916828485, 15917747678, 15918788082, 15919818385, 15920848687, 15921666870, 15922909193, 15923858688, 15924757779, 15925596164, 15926515356, 15927838587, 10901848687, 10902777980, 10903798182, 10904767879, 10905798082, 10906818384, 10907777880, 10908717375, 10909656769, 10910687072, 10911687072, 10912818384, 10913747677, 10914687072, 10915656769, 10916757678, 10917767880, 10918818384, 10919858788, 10920848687, 10921616365, 10922828486, 10923788082, 10924707274, 10925596163, 10926545658, 10927788081, 15201868890, 15202828587, 15203828487, 15204778083, 15205808285, 15206828487, 15207777982, 15208677073, 15209687174, 15210757881, 15211737679, 15212828487, 15213757880, 15214727578, 15215596366, 15216697275, 15217646770, 15218798284, 15219838688, 15220848688, 15221666972, 15222818486, 15223858789, 15224737679, 15225525558, 15226606366, 15227788184, 15301868889, 15302798183, 15303838486, 15304828486, 15305838586, 15306858789, 15307828486, 15308747678, 15309717375, 15310717476, 15311747678, 15312798182, 15313767880, 15314737577, 15315596163, 15316717375, 15317687072, 15318838587, 15319889091, 15320868889, 15321707274, 15322868789, 15323848688, 15324747678, 15325606365, 15326555759, 15327798182, 15401889092, 15402858789, 15403848688, 15404818385, 15405838587, 15406858789, 15407838587, 15408778082, 15409757880, 15410777981, 15411778082, 15412838587, 15413818385, 15414788082, 15415697275, 15416737678, 15417777981, 15418868890, 15419909293, 15420889092, 15421737578, 15422858789, 15423878991, 15424808284, 15425677072, 15426626467, 15427838587, 16101879093, 16102848891, 16103818589, 16104798487, 16105808487, 16106828689, 16107778185, 16108798387, 16109768084, 16110707579, 16111757983, 16112828690, 16113778285, 16114737882, 16115586368, 16116828689, 16117687378, 16118717680, 16119788286, 16120798487, 16121717680, 16122848891, 16123868992, 16124737882, 16125616671, 16126576367, 16127768184, 15501919293, 15502858788, 15503878990, 15504787981, 15505848687, 15506848687, 15507777980, 15508687173, 15509717375, 15510727476, 15511697173, 15512888990, 15513767880, 15514737577, 15515707274, 15516808284, 15517808284, 15518868889, 15519888991, 15520889091, 15521646668, 15522838486, 15523858788, 15524747677, 15525535557, 15526414346, 15527848587, 16201909294, 16202868990, 16203828486, 16204788183, 16205848688, 16206848688, 16207818486, 16208757881, 16209757780, 16210848688, 16211798184, 16212878991, 16213838588, 16214788183, 16215737679, 16216757880, 16217727578, 16218828587, 16219889092, 16220858789, 16221727578, 16222828587, 16223878991, 16224777982, 16225636669, 16226606366, 16227838588, 5101778591, 5102758389, 5103748288, 5104687784, 5105738288, 5106849195, 5107758389, 5108546472, 5109586775, 5110475665, 5111556573, 5112606977, 5113536271, 5114556473, 5115495867, 5116606977, 5117627179, 5118697885, 5119778591, 5120758389, 5121455564, 5122818893, 5123798792, 5124627279, 5125617078, 5126354454, 5127717986]);
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Update$getOverallData = F2(
	function (qList, uniList) {
		var uniS = $elm$core$Set$fromList(uniList);
		var qS = $elm$core$Set$fromList(qList);
		var f = function (i) {
			return A3(
				$author$project$Update$matching,
				qS,
				uniS,
				$author$project$Update$intToNss(i));
		};
		return A2(
			$elm$core$List$map,
			$elm$core$Tuple$second,
			A2(
				$elm$core$List$sortBy,
				$elm$core$Tuple$first,
				A2(
					$elm$core$List$map,
					A2($elm$core$Basics$composeL, $author$project$Update$getDataPoint, $author$project$Update$intToNss),
					A2($elm$core$List$filter, f, $author$project$Data$nss))));
	});
var $author$project$Update$getDataPoint2 = function (_v0) {
	var uni = _v0.aE;
	var min = _v0.aj;
	var value = _v0.aF;
	var max = _v0.ai;
	return _Utils_Tuple2(
		uni,
		_Utils_Tuple3(min, value, max));
};
var $author$project$Update$intToNss2 = function (i) {
	return {
		ai: $author$project$Update$unwrap(
			$elm$core$Basics$floor(
				A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 100, i))),
		aj: $author$project$Update$unwrap(
			$elm$core$Basics$floor(
				A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 100, i / 10000))),
		as: $elm$core$Basics$floor(
			A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 100, i / 1000000)),
		bl: $elm$core$Basics$floor(
			A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 1000, i / 100000000)),
		aE: $elm$core$Basics$floor(
			A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 1000, i / 100000000000)),
		aF: $author$project$Update$unwrap(
			$elm$core$Basics$floor(
				A2($elm_community$basics_extra$Basics$Extra$fractionalModBy, 100, i / 100)))
	};
};
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $author$project$Update$matching2 = F4(
	function (subjectCandidate, qs, unis, _v0) {
		var uni = _v0.aE;
		var subject = _v0.bl;
		var q = _v0.as;
		return A2(
			$elm$core$List$all,
			function (x) {
				return x;
			},
			_List_fromArray(
				[
					_Utils_eq(subject, subjectCandidate),
					A2($elm$core$Set$member, uni, unis),
					A2($elm$core$Set$member, q, qs)
				]));
	});
var $author$project$Data$nss2 = _List_fromArray(
var $author$project$Update$getSubjectData = F3(
	function (subject, questionList, uniList) {
		var uniS = $elm$core$Set$fromList(uniList);
		var qS = $elm$core$Set$fromList(questionList);
		var f = function (i) {
			return A4(
				$author$project$Update$matching2,
				subject,
				qS,
				uniS,
				$author$project$Update$intToNss2(i));
		};
		return A2(
			$elm$core$List$map,
			$elm$core$Tuple$second,
			A2(
				$elm$core$List$sortBy,
				$elm$core$Tuple$first,
				A2(
					$elm$core$List$map,
					A2($elm$core$Basics$composeL, $author$project$Update$getDataPoint2, $author$project$Update$intToNss2),
					A2($elm$core$List$filter, f, $author$project$Data$nss2))));
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Update$freshModel = function (model) {
	var questions = model.bd;
	var unis = model.bv;
	if (A2(
		$elm$core$List$any,
		$elm$core$List$isEmpty,
		_List_fromArray(
			[questions, unis]))) {
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	} else {
		var _v0 = model.X;
		if (_v0.$ === 1) {
			if (_v0.a.$ === 1) {
				var _v1 = _v0.a;
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			} else {
				var subj = _v0.a.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aS: A3($author$project$Update$getSubjectData, subj, questions, unis)
						}),
					$elm$core$Platform$Cmd$none);
			}
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						aS: A2($author$project$Update$getOverallData, questions, unis)
					}),
				$elm$core$Platform$Cmd$none);
		}
	}
};
var $author$project$DataTypes$FoldedUp = {$: 3};
var $author$project$DataTypes$Overall = {$: 0};
var $author$project$DataTypes$UniVsA = 0;
var $rluiten$elm_text_search$Index$Model$Index = $elm$core$Basics$identity;
var $rluiten$trie$TrieModel$EmptyTrie = {$: 0};
var $rluiten$trie$TrieModel$TrieNode = function (a) {
	return {$: 2, a: a};
};
var $rluiten$trie$TrieModel$ValNode = function (a) {
	return {$: 1, a: a};
};
var $rluiten$trie$TrieModel$ValTrieNode = function (a) {
	return {$: 3, a: a};
};
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $rluiten$trie$TrieModel$addByStr = F3(
	function (_v0, key, trie) {
		var ref = _v0.a;
		var value = _v0.b;
		if (!key.b) {
			switch (trie.$) {
				case 0:
					return $rluiten$trie$TrieModel$ValNode(
						A2($elm$core$Dict$singleton, ref, value));
				case 1:
					var refValues = trie.a;
					return $rluiten$trie$TrieModel$ValNode(
						A3($elm$core$Dict$insert, ref, value, refValues));
				case 2:
					var trieDict = trie.a;
					return $rluiten$trie$TrieModel$ValTrieNode(
						_Utils_Tuple2(
							A2($elm$core$Dict$singleton, ref, value),
							trieDict));
				default:
					var _v3 = trie.a;
					var refValues = _v3.a;
					var trieDict = _v3.b;
					return $rluiten$trie$TrieModel$ValTrieNode(
						_Utils_Tuple2(
							A3($elm$core$Dict$insert, ref, value, refValues),
							trieDict));
			}
		} else {
			var keyHead = key.a;
			var keyTail = key.b;
			var updateTrieDict = function (trieDict) {
				var updatedSubTrie = A3(
					$rluiten$trie$TrieModel$addByStr,
					_Utils_Tuple2(ref, value),
					keyTail,
					A2(
						$elm$core$Maybe$withDefault,
						$rluiten$trie$TrieModel$EmptyTrie,
						A2($elm$core$Dict$get, keyHead, trieDict)));
				return A3($elm$core$Dict$insert, keyHead, updatedSubTrie, trieDict);
			};
			var lazyNewTrieDict = function (_v6) {
				return A2(
					$elm$core$Dict$singleton,
					keyHead,
					A3(
						$rluiten$trie$TrieModel$addByStr,
						_Utils_Tuple2(ref, value),
						keyTail,
						$rluiten$trie$TrieModel$EmptyTrie));
			};
			switch (trie.$) {
				case 0:
					return $rluiten$trie$TrieModel$TrieNode(
						lazyNewTrieDict(0));
				case 1:
					var refValues = trie.a;
					return $rluiten$trie$TrieModel$ValTrieNode(
						_Utils_Tuple2(
							refValues,
							lazyNewTrieDict(0)));
				case 2:
					var trieDict = trie.a;
					return $rluiten$trie$TrieModel$TrieNode(
						updateTrieDict(trieDict));
				default:
					var _v5 = trie.a;
					var refValues = _v5.a;
					var trieDict = _v5.b;
					return $rluiten$trie$TrieModel$ValTrieNode(
						_Utils_Tuple2(
							refValues,
							updateTrieDict(trieDict)));
			}
		}
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $rluiten$trie$TrieModel$toListString = function (str) {
	return A2(
		$elm$core$List$map,
		function (c) {
			return $elm$core$String$fromChar(c);
		},
		$elm$core$String$toList(str));
};
var $rluiten$trie$TrieModel$add = F3(
	function (refValues, key, trie) {
		return A3(
			$rluiten$trie$TrieModel$addByStr,
			refValues,
			$rluiten$trie$TrieModel$toListString(key),
			trie);
	});
var $rluiten$trie$Trie$add = $rluiten$trie$TrieModel$add;
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $rluiten$elm_text_search$Index$Utils$buildOrderIndex = function (tokenSet) {
	var withIndex = A2(
		$elm$core$List$indexedMap,
		$elm$core$Tuple$pair,
		$elm$core$Set$toList(tokenSet));
	return A3(
		$elm$core$List$foldr,
		F2(
			function (_v0, d) {
				var i = _v0.a;
				var v = _v0.b;
				return A3($elm$core$Dict$insert, v, i, d);
			}),
		$elm$core$Dict$empty,
		withIndex);
};
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === -2) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Set$isEmpty = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$isEmpty(dict);
};
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === -2) {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $elm$core$Set$size = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$size(dict);
};
var $rluiten$elm_text_search$Index$scoreToken = F2(
	function (fieldTokensAndBoost, token) {
		var score = F2(
			function (_v0, scoreSum) {
				var tokenSet = _v0.a;
				var fieldBoost = _v0.b;
				if ($elm$core$Set$isEmpty(tokenSet)) {
					return scoreSum;
				} else {
					var tokenBoost = A2($elm$core$Set$member, token, tokenSet) ? (fieldBoost / $elm$core$Set$size(tokenSet)) : 0;
					return scoreSum + tokenBoost;
				}
			});
		return _Utils_Tuple2(
			token,
			A3($elm$core$List$foldr, score, 0, fieldTokensAndBoost));
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$core$Set$union = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$union, dict1, dict2);
	});
var $rluiten$elm_text_search$Index$addDoc = F4(
	function (docRef, fieldsTokens, docTokens, index) {
		var irec = index;
		var updatedDocumentStore = A3($elm$core$Dict$insert, docRef, docTokens, irec.H);
		var updatedCorpusTokens = A2($elm$core$Set$union, irec.G, docTokens);
		var updatedCorpusTokensIndex = $rluiten$elm_text_search$Index$Utils$buildOrderIndex(updatedCorpusTokens);
		var allBoosts = A2(
			$elm$core$List$append,
			A2($elm$core$List$map, $elm$core$Tuple$second, irec.a4),
			A2($elm$core$List$map, $elm$core$Tuple$second, irec.aY));
		var fieldTokensAndBoosts = A3($elm$core$List$map2, $elm$core$Tuple$pair, fieldsTokens, allBoosts);
		var tokenAndScores = A2(
			$elm$core$List$map,
			$rluiten$elm_text_search$Index$scoreToken(fieldTokensAndBoosts),
			$elm$core$Set$toList(docTokens));
		var addTokenScore = F2(
			function (_v0, trie) {
				var token = _v0.a;
				var score = _v0.b;
				return A3(
					$rluiten$trie$Trie$add,
					_Utils_Tuple2(docRef, score),
					token,
					trie);
			});
		var updatedTokenStore = A3($elm$core$List$foldr, addTokenScore, irec.T, tokenAndScores);
		return _Utils_update(
			irec,
			{G: updatedCorpusTokens, Y: updatedCorpusTokensIndex, H: updatedDocumentStore, J: $elm$core$Dict$empty, T: updatedTokenStore});
	});
var $rluiten$elm_text_search$Index$Utils$applyFilterList = F2(
	function (filters, token) {
		applyFilterList:
		while (true) {
			if (!filters.b) {
				return true;
			} else {
				var filterFunc = filters.a;
				var restFilters = filters.b;
				if (token === '') {
					return false;
				} else {
					var _v2 = filterFunc(token);
					if (!_v2) {
						return false;
					} else {
						var $temp$filters = restFilters,
							$temp$token = token;
						filters = $temp$filters;
						token = $temp$token;
						continue applyFilterList;
					}
				}
			}
		}
	});
var $rluiten$elm_text_search$Index$Utils$runFactories = F2(
	function (factoryList, index) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (factory, _v0) {
					var u1index = _v0.a;
					var funcList = _v0.b;
					var _v1 = factory(u1index);
					var u2index = _v1.a;
					var newFunc = _v1.b;
					return _Utils_Tuple2(
						u2index,
						A2($elm$core$List$cons, newFunc, funcList));
				}),
			_Utils_Tuple2(index, _List_Nil),
			factoryList);
	});
var $rluiten$elm_text_search$Index$Utils$getOrSetIndexFuncList = F4(
	function (getFuncs, getFactoryFuncs, setFuncs, index) {
		var _v0 = getFuncs(index);
		if (!_v0.$) {
			var funcList = _v0.a;
			return _Utils_Tuple2(index, funcList);
		} else {
			var _v1 = A2(
				$rluiten$elm_text_search$Index$Utils$runFactories,
				getFactoryFuncs(index),
				index);
			var u1index = _v1.a;
			var newFuncList = _v1.b;
			var u2index = A2(setFuncs, u1index, newFuncList);
			return _Utils_Tuple2(u2index, newFuncList);
		}
	});
var $rluiten$elm_text_search$Index$Utils$setIndexFilters = F2(
	function (_v0, listFuncs) {
		var irec = _v0;
		return _Utils_update(
			irec,
			{
				aZ: $elm$core$Maybe$Just(listFuncs)
			});
	});
var $rluiten$elm_text_search$Index$Utils$getOrSetFilterList = function (index) {
	return A4(
		$rluiten$elm_text_search$Index$Utils$getOrSetIndexFuncList,
		function (_v0) {
			var irec = _v0;
			return irec.aZ;
		},
		function (_v1) {
			var irec = _v1;
			return irec.ab;
		},
		$rluiten$elm_text_search$Index$Utils$setIndexFilters,
		index);
};
var $rluiten$elm_text_search$Index$Utils$applyFilter = F2(
	function (index, strings) {
		var _v0 = $rluiten$elm_text_search$Index$Utils$getOrSetFilterList(index);
		var u1index = _v0.a;
		var filterList = _v0.b;
		return _Utils_Tuple2(
			u1index,
			A2(
				$elm$core$List$filter,
				$rluiten$elm_text_search$Index$Utils$applyFilterList(filterList),
				strings));
	});
var $rluiten$elm_text_search$Index$Utils$applyTransformList = F2(
	function (transforms, token) {
		applyTransformList:
		while (true) {
			if (!transforms.b) {
				return token;
			} else {
				var transform = transforms.a;
				var restTransforms = transforms.b;
				var newToken = transform(token);
				if (newToken === '') {
					return '';
				} else {
					var $temp$transforms = restTransforms,
						$temp$token = newToken;
					transforms = $temp$transforms;
					token = $temp$token;
					continue applyTransformList;
				}
			}
		}
	});
var $rluiten$elm_text_search$Index$Utils$setIndexInitialTransforms = F2(
	function (_v0, listFuncs) {
		var irec = _v0;
		return _Utils_update(
			irec,
			{
				a3: $elm$core$Maybe$Just(listFuncs)
			});
	});
var $rluiten$elm_text_search$Index$Utils$getOrSetInitialTransformList = function (index) {
	return A4(
		$rluiten$elm_text_search$Index$Utils$getOrSetIndexFuncList,
		function (_v0) {
			var irec = _v0;
			return irec.a3;
		},
		function (_v1) {
			var irec = _v1;
			return irec.ag;
		},
		$rluiten$elm_text_search$Index$Utils$setIndexInitialTransforms,
		index);
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $rluiten$elm_text_search$Index$Utils$applyInitialTransform = F2(
	function (index, strings) {
		var _v0 = $rluiten$elm_text_search$Index$Utils$getOrSetInitialTransformList(index);
		var u1index = _v0.a;
		var intitialTransformList = _v0.b;
		return _Utils_Tuple2(
			u1index,
			A2(
				$elm$core$List$filter,
				function (val) {
					return val !== '';
				},
				A2(
					$elm$core$List$map,
					$rluiten$elm_text_search$Index$Utils$applyTransformList(intitialTransformList),
					strings)));
	});
var $rluiten$elm_text_search$Index$Utils$setIndexTransforms = F2(
	function (_v0, listFuncs) {
		var irec = _v0;
		return _Utils_update(
			irec,
			{
				bs: $elm$core$Maybe$Just(listFuncs)
			});
	});
var $rluiten$elm_text_search$Index$Utils$getOrSetTransformList = function (index) {
	return A4(
		$rluiten$elm_text_search$Index$Utils$getOrSetIndexFuncList,
		function (_v0) {
			var irec = _v0;
			return irec.bs;
		},
		function (_v1) {
			var irec = _v1;
			return irec.aD;
		},
		$rluiten$elm_text_search$Index$Utils$setIndexTransforms,
		index);
};
var $rluiten$elm_text_search$Index$Utils$applyTransform = F2(
	function (index, strings) {
		var _v0 = $rluiten$elm_text_search$Index$Utils$getOrSetTransformList(index);
		var u1index = _v0.a;
		var transformList = _v0.b;
		return _Utils_Tuple2(
			u1index,
			A2(
				$elm$core$List$filter,
				function (val) {
					return val !== '';
				},
				A2(
					$elm$core$List$map,
					$rluiten$elm_text_search$Index$Utils$applyTransformList(transformList),
					strings)));
	});
var $rluiten$elm_text_search$Index$Utils$processTokens = F2(
	function (index, tokens) {
		var _v0 = A2($rluiten$elm_text_search$Index$Utils$applyInitialTransform, index, tokens);
		var u1index = _v0.a;
		var initialTransformTokens = _v0.b;
		var _v1 = A2($rluiten$elm_text_search$Index$Utils$applyFilter, u1index, initialTransformTokens);
		var u2index = _v1.a;
		var filterTokens = _v1.b;
		return A2($rluiten$elm_text_search$Index$Utils$applyTransform, u2index, filterTokens);
	});
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {a1: index, a5: match, a8: number, bo: submatches};
	});
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{aP: false, a7: false},
		string);
};
var $elm$regex$Regex$never = _Regex_never;
var $rluiten$elm_text_search$TokenProcessors$forceRegex = A2(
	$elm$core$Basics$composeL,
	$elm$core$Maybe$withDefault($elm$regex$Regex$never),
	$elm$regex$Regex$fromString);
var $rluiten$elm_text_search$TokenProcessors$defaultSeparator = $rluiten$elm_text_search$TokenProcessors$forceRegex('[\\s\\-]+');
var $elm$regex$Regex$split = _Regex_splitAtMost(_Regex_infinity);
var $elm$core$String$toLower = _String_toLower;
var $elm$core$String$trim = _String_trim;
var $rluiten$elm_text_search$TokenProcessors$tokenizerWithRegex = F2(
	function (seperatorRegex, data) {
		var splitter = A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				$elm$regex$Regex$split(seperatorRegex),
				$elm$core$String$toLower),
			$elm$core$String$trim);
		return A2(
			$elm$core$List$filter,
			function (token) {
				return $elm$core$String$length(token) > 0;
			},
			splitter(data));
	});
var $rluiten$elm_text_search$TokenProcessors$tokenizer = $rluiten$elm_text_search$TokenProcessors$tokenizerWithRegex($rluiten$elm_text_search$TokenProcessors$defaultSeparator);
var $rluiten$elm_text_search$Index$Utils$getTokens = F2(
	function (index, string) {
		return A2(
			$rluiten$elm_text_search$Index$Utils$processTokens,
			index,
			$rluiten$elm_text_search$TokenProcessors$tokenizer(string));
	});
var $rluiten$elm_text_search$Index$getWordsForField = F3(
	function (doc, getField, _v0) {
		var index = _v0.a;
		var fieldsLists = _v0.b;
		var _v1 = A2(
			$rluiten$elm_text_search$Index$Utils$getTokens,
			index,
			getField(doc));
		var u1index = _v1.a;
		var tokens = _v1.b;
		return _Utils_Tuple2(
			u1index,
			A2($elm$core$List$cons, tokens, fieldsLists));
	});
var $rluiten$elm_text_search$TokenProcessors$tokenizerWithRegexList = F2(
	function (seperatorRegex, listData) {
		var splitter = A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				$elm$regex$Regex$split(seperatorRegex),
				$elm$core$String$toLower),
			$elm$core$String$trim);
		var tokens = A3(
			$elm$core$List$foldr,
			F2(
				function (str, agg) {
					return A2(
						$elm$core$List$append,
						agg,
						splitter(str));
				}),
			_List_Nil,
			listData);
		return A2(
			$elm$core$List$filter,
			function (token) {
				return $elm$core$String$length(token) > 0;
			},
			tokens);
	});
var $rluiten$elm_text_search$TokenProcessors$tokenizerList = $rluiten$elm_text_search$TokenProcessors$tokenizerWithRegexList($rluiten$elm_text_search$TokenProcessors$defaultSeparator);
var $rluiten$elm_text_search$Index$Utils$getTokensList = F2(
	function (index, listString) {
		return A2(
			$rluiten$elm_text_search$Index$Utils$processTokens,
			index,
			$rluiten$elm_text_search$TokenProcessors$tokenizerList(listString));
	});
var $rluiten$elm_text_search$Index$getWordsForFieldList = F3(
	function (doc, getFieldList, _v0) {
		var index = _v0.a;
		var fieldsLists = _v0.b;
		var _v1 = A2(
			$rluiten$elm_text_search$Index$Utils$getTokensList,
			index,
			getFieldList(doc));
		var u1index = _v1.a;
		var tokens = _v1.b;
		return _Utils_Tuple2(
			u1index,
			A2($elm$core$List$cons, tokens, fieldsLists));
	});
var $rluiten$elm_text_search$Index$Utils$refExists = F2(
	function (docRef, _v0) {
		var irec = _v0;
		return A2($elm$core$Dict$member, docRef, irec.H);
	});
var $rluiten$elm_text_search$Index$add = F2(
	function (doc, index) {
		var irec = index;
		var docRef = irec.bf(doc);
		if ($elm$core$String$isEmpty(docRef)) {
			return $elm$core$Result$Err('Error document has an empty unique id (ref).');
		} else {
			if (A2($rluiten$elm_text_search$Index$Utils$refExists, docRef, index)) {
				return $elm$core$Result$Err('Error adding document that allready exists.');
			} else {
				var _v0 = A3(
					$elm$core$List$foldr,
					$rluiten$elm_text_search$Index$getWordsForField(doc),
					_Utils_Tuple2(index, _List_Nil),
					A2($elm$core$List$map, $elm$core$Tuple$first, irec.aY));
				var u1index = _v0.a;
				var fieldsWordList = _v0.b;
				var _v1 = A3(
					$elm$core$List$foldr,
					$rluiten$elm_text_search$Index$getWordsForFieldList(doc),
					_Utils_Tuple2(u1index, fieldsWordList),
					A2($elm$core$List$map, $elm$core$Tuple$first, irec.a4));
				var u2index = _v1.a;
				var u2fieldsWordList = _v1.b;
				var fieldsTokens = A2($elm$core$List$map, $elm$core$Set$fromList, u2fieldsWordList);
				var docTokens = A3($elm$core$List$foldr, $elm$core$Set$union, $elm$core$Set$empty, fieldsTokens);
				return $elm$core$Set$isEmpty(docTokens) ? $elm$core$Result$Err('Error after tokenisation there are no terms to index.') : $elm$core$Result$Ok(
					A4($rluiten$elm_text_search$Index$addDoc, docRef, fieldsTokens, docTokens, u2index));
			}
		}
	});
var $rluiten$elm_text_search$Index$addDocsCore = F4(
	function (docsI, docs, index, errors) {
		addDocsCore:
		while (true) {
			var irec = index;
			if (!docs.b) {
				return _Utils_Tuple2(index, errors);
			} else {
				var headDoc = docs.a;
				var tailDocs = docs.b;
				var _v1 = A2($rluiten$elm_text_search$Index$add, headDoc, index);
				if (!_v1.$) {
					var u1index = _v1.a;
					var $temp$docsI = docsI + 1,
						$temp$docs = tailDocs,
						$temp$index = u1index,
						$temp$errors = errors;
					docsI = $temp$docsI;
					docs = $temp$docs;
					index = $temp$index;
					errors = $temp$errors;
					continue addDocsCore;
				} else {
					var msg = _v1.a;
					var $temp$docsI = docsI + 1,
						$temp$docs = tailDocs,
						$temp$index = index,
						$temp$errors = _Utils_ap(
						errors,
						_List_fromArray(
							[
								_Utils_Tuple2(docsI, msg)
							]));
					docsI = $temp$docsI;
					docs = $temp$docs;
					index = $temp$index;
					errors = $temp$errors;
					continue addDocsCore;
				}
			}
		}
	});
var $rluiten$elm_text_search$Index$addDocs = F2(
	function (docs, index) {
		return A4($rluiten$elm_text_search$Index$addDocsCore, 0, docs, index, _List_Nil);
	});
var $rluiten$elm_text_search$ElmTextSearch$addDocs = $rluiten$elm_text_search$Index$addDocs;
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Update$indexContent = function (codes) {
	return A2(
		$elm$core$List$map,
		function (x) {
			return {N: x};
		},
		$elm$core$Dict$values(codes));
};
var $rluiten$elm_text_search$Index$Defaults$elmTextSearchIndexType = '-= ElmTextSearch Index Type 1 =-';
var $rluiten$elm_text_search$Index$Defaults$getIndexSimpleConfig = function (_v0) {
	var ref = _v0.bf;
	var fields = _v0.aY;
	var listFields = _v0.a4;
	return {aY: fields, K: $rluiten$elm_text_search$Index$Defaults$elmTextSearchIndexType, a4: listFields, bf: ref};
};
var $rluiten$elm_text_search$StopWordFilter$createFilterFunc = F2(
	function (tokens, index) {
		var tokenSet = $elm$core$Set$fromList(tokens);
		return _Utils_Tuple2(
			index,
			function (word) {
				return !A2($elm$core$Set$member, word, tokenSet);
			});
	});
var $rluiten$elm_text_search$StopWordFilter$stopEnglishWordList = _List_fromArray(
	['a', 'able', 'about', 'across', 'after', 'all', 'almost', 'also', 'am', 'among', 'an', 'and', 'any', 'are', 'as', 'at', 'be', 'because', 'been', 'but', 'by', 'can', 'cannot', 'could', 'dear', 'did', 'do', 'does', 'either', 'else', 'ever', 'every', 'for', 'from', 'get', 'got', 'had', 'has', 'have', 'he', 'her', 'hers', 'him', 'his', 'how', 'however', 'i', 'if', 'in', 'into', 'is', 'it', 'its', 'just', 'least', 'let', 'like', 'likely', 'may', 'me', 'might', 'most', 'must', 'my', 'neither', 'no', 'nor', 'not', 'of', 'off', 'often', 'on', 'only', 'or', 'other', 'our', 'own', 'rather', 'said', 'say', 'says', 'she', 'should', 'since', 'so', 'some', 'than', 'that', 'the', 'their', 'them', 'then', 'there', 'these', 'they', 'this', 'tis', 'to', 'too', 'twas', 'us', 'wants', 'was', 'we', 'were', 'what', 'when', 'where', 'which', 'while', 'who', 'whom', 'why', 'will', 'with', 'would', 'yet', 'you', 'your']);
var $rluiten$elm_text_search$StopWordFilter$createDefaultFilterFunc = function (index) {
	return A2($rluiten$elm_text_search$StopWordFilter$createFilterFunc, $rluiten$elm_text_search$StopWordFilter$stopEnglishWordList, index);
};
var $rluiten$elm_text_search$Index$Defaults$defaultStopWordFilterFuncCreator = $rluiten$elm_text_search$StopWordFilter$createDefaultFilterFunc;
var $rluiten$elm_text_search$Index$Defaults$defaultFilterFactories = _List_fromArray(
	[$rluiten$elm_text_search$Index$Defaults$defaultStopWordFilterFuncCreator]);
var $rluiten$elm_text_search$Index$Utils$createFuncCreator = F2(
	function (func, index) {
		return _Utils_Tuple2(index, func);
	});
var $elm$regex$Regex$replace = _Regex_replaceAtMost(_Regex_infinity);
var $rluiten$elm_text_search$TokenProcessors$trimmerRegex = $rluiten$elm_text_search$TokenProcessors$forceRegex('^\\W+|\\W+$');
var $rluiten$elm_text_search$TokenProcessors$trimmer = A2(
	$elm$regex$Regex$replace,
	$rluiten$elm_text_search$TokenProcessors$trimmerRegex,
	function (_v0) {
		return '';
	});
var $rluiten$elm_text_search$Index$Defaults$defaultTokenTrimmerFuncCreator = $rluiten$elm_text_search$Index$Utils$createFuncCreator($rluiten$elm_text_search$TokenProcessors$trimmer);
var $rluiten$elm_text_search$Index$Defaults$defaultInitialTransformFactories = _List_fromArray(
	[$rluiten$elm_text_search$Index$Defaults$defaultTokenTrimmerFuncCreator]);
var $elm$core$String$reverse = _String_reverse;
var $rluiten$stemmer$Stemmer$step1aX = function (drow) {
	return A2($elm$core$String$startsWith, 'sess', drow) ? A2($elm$core$String$dropLeft, 2, drow) : (A2($elm$core$String$startsWith, 'sei', drow) ? A2($elm$core$String$dropLeft, 2, drow) : (A2($elm$core$String$startsWith, 'ss', drow) ? drow : (A2($elm$core$String$startsWith, 's', drow) ? A2($elm$core$String$dropLeft, 1, drow) : drow)));
};
var $rluiten$stemmer$Stemmer$isVowelCore = F2(
	function (includeY, c) {
		switch (c) {
			case 'a':
				return true;
			case 'e':
				return true;
			case 'i':
				return true;
			case 'o':
				return true;
			case 'u':
				return true;
			case 'y':
				return includeY ? true : false;
			default:
				return false;
		}
	});
var $rluiten$stemmer$Stemmer$isVowelWithY = $rluiten$stemmer$Stemmer$isVowelCore(true);
var $rluiten$stemmer$Stemmer$hasVowel2X = function (word) {
	hasVowel2X:
	while (true) {
		var _v0 = $elm$core$String$uncons(word);
		if (!_v0.$) {
			var _v1 = _v0.a;
			var h = _v1.a;
			var wordTail = _v1.b;
			var _v2 = $rluiten$stemmer$Stemmer$isVowelWithY(h);
			if (_v2) {
				return true;
			} else {
				var $temp$word = wordTail;
				word = $temp$word;
				continue hasVowel2X;
			}
		} else {
			return false;
		}
	}
};
var $rluiten$stemmer$Stemmer$isVowel = $rluiten$stemmer$Stemmer$isVowelCore(false);
var $rluiten$stemmer$Stemmer$hasVowelX = function (drow) {
	var _v0 = $elm$core$String$uncons(
		$elm$core$String$reverse(drow));
	if (!_v0.$) {
		var _v1 = _v0.a;
		var h = _v1.a;
		var wordTail = _v1.b;
		var _v2 = $rluiten$stemmer$Stemmer$isVowel(h);
		if (_v2) {
			return true;
		} else {
			return $rluiten$stemmer$Stemmer$hasVowel2X(wordTail);
		}
	} else {
		return false;
	}
};
var $rluiten$stemmer$Stemmer$foundConsonantX = F2(
	function (word, m) {
		foundConsonantX:
		while (true) {
			var _v3 = $elm$core$String$uncons(word);
			if (!_v3.$) {
				var _v4 = _v3.a;
				var h = _v4.a;
				var wordTail = _v4.b;
				var _v5 = $rluiten$stemmer$Stemmer$isVowelWithY(h);
				if (_v5) {
					return A2($rluiten$stemmer$Stemmer$foundVowelX, wordTail, m);
				} else {
					var $temp$word = wordTail,
						$temp$m = m;
					word = $temp$word;
					m = $temp$m;
					continue foundConsonantX;
				}
			} else {
				return m;
			}
		}
	});
var $rluiten$stemmer$Stemmer$foundVowelX = F2(
	function (word, m) {
		foundVowelX:
		while (true) {
			var _v0 = $elm$core$String$uncons(word);
			if (!_v0.$) {
				var _v1 = _v0.a;
				var h = _v1.a;
				var wordTail = _v1.b;
				var _v2 = $rluiten$stemmer$Stemmer$isVowel(h);
				if (_v2) {
					var $temp$word = wordTail,
						$temp$m = m;
					word = $temp$word;
					m = $temp$m;
					continue foundVowelX;
				} else {
					return A2($rluiten$stemmer$Stemmer$foundConsonantX, wordTail, m + 1);
				}
			} else {
				return m;
			}
		}
	});
var $rluiten$stemmer$Stemmer$foundLeadingConsonantX = function (word) {
	foundLeadingConsonantX:
	while (true) {
		var _v0 = $elm$core$String$uncons(word);
		if (!_v0.$) {
			var _v1 = _v0.a;
			var h = _v1.a;
			var wordTail = _v1.b;
			var _v2 = $rluiten$stemmer$Stemmer$isVowelWithY(h);
			if (_v2) {
				return A2($rluiten$stemmer$Stemmer$foundVowelX, wordTail, 0);
			} else {
				var $temp$word = wordTail;
				word = $temp$word;
				continue foundLeadingConsonantX;
			}
		} else {
			return 0;
		}
	}
};
var $rluiten$stemmer$Stemmer$measureX = function (drow) {
	var word = $elm$core$String$reverse(drow);
	var _v0 = $elm$core$String$uncons(word);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var h = _v1.a;
		var wordTail = _v1.b;
		var _v2 = $rluiten$stemmer$Stemmer$isVowel(h);
		if (_v2) {
			return A2($rluiten$stemmer$Stemmer$foundVowelX, wordTail, 0);
		} else {
			return $rluiten$stemmer$Stemmer$foundLeadingConsonantX(wordTail);
		}
	} else {
		return 0;
	}
};
var $rluiten$stemmer$Stemmer$endsWithCVCX = function (drow) {
	var _v0 = $elm$core$String$uncons(drow);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var c2 = _v1.a;
		var drowTail1 = _v1.b;
		if (!($rluiten$stemmer$Stemmer$isVowel(c2) || ((c2 === 'w') || ((c2 === 'x') || (c2 === 'y'))))) {
			var _v2 = $elm$core$String$uncons(drowTail1);
			if (!_v2.$) {
				var _v3 = _v2.a;
				var v = _v3.a;
				var drowTail2 = _v3.b;
				if ($rluiten$stemmer$Stemmer$isVowelWithY(v)) {
					var _v4 = $elm$core$String$uncons(drowTail2);
					if (!_v4.$) {
						var _v5 = _v4.a;
						var c1 = _v5.a;
						var drowTail3 = _v5.b;
						return !$rluiten$stemmer$Stemmer$isVowel(c1);
					} else {
						return false;
					}
				} else {
					return false;
				}
			} else {
				return false;
			}
		} else {
			return false;
		}
	} else {
		return false;
	}
};
var $rluiten$stemmer$Stemmer$endsWithDoubleConsX = function (drow) {
	var _v0 = $elm$core$String$uncons(drow);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var c1 = _v1.a;
		var drowTail = _v1.b;
		if (!$rluiten$stemmer$Stemmer$isVowelWithY(c1)) {
			var _v2 = $elm$core$String$uncons(drowTail);
			if (!_v2.$) {
				var _v3 = _v2.a;
				var c2 = _v3.a;
				var drowTail2 = _v3.b;
				return _Utils_eq(c1, c2);
			} else {
				return false;
			}
		} else {
			return false;
		}
	} else {
		return false;
	}
};
var $rluiten$stemmer$Stemmer$step1b2X = function (drow) {
	if (A2($elm$core$String$startsWith, 'ta', drow) || (A2($elm$core$String$startsWith, 'lb', drow) || A2($elm$core$String$startsWith, 'zi', drow))) {
		return A2($elm$core$String$cons, 'e', drow);
	} else {
		var _v0 = $elm$core$String$uncons(drow);
		if (!_v0.$) {
			var _v1 = _v0.a;
			var h = _v1.a;
			var drowTail = _v1.b;
			return ($rluiten$stemmer$Stemmer$endsWithDoubleConsX(drow) && (!((h === 'l') || ((h === 's') || (h === 'z'))))) ? drowTail : ((($rluiten$stemmer$Stemmer$measureX(drow) === 1) && $rluiten$stemmer$Stemmer$endsWithCVCX(drow)) ? A2($elm$core$String$cons, 'e', drow) : drow);
		} else {
			return drow;
		}
	}
};
var $rluiten$stemmer$Stemmer$step1bX = function (drow) {
	if (A2($elm$core$String$startsWith, 'dee', drow)) {
		return ($rluiten$stemmer$Stemmer$measureX(
			A2($elm$core$String$dropLeft, 3, drow)) > 0) ? A2($elm$core$String$dropLeft, 1, drow) : drow;
	} else {
		if (A2($elm$core$String$startsWith, 'de', drow)) {
			var mets = A2($elm$core$String$dropLeft, 2, drow);
			return $rluiten$stemmer$Stemmer$hasVowelX(mets) ? $rluiten$stemmer$Stemmer$step1b2X(mets) : drow;
		} else {
			if (A2($elm$core$String$startsWith, 'gni', drow)) {
				var mets = A2($elm$core$String$dropLeft, 3, drow);
				return $rluiten$stemmer$Stemmer$hasVowelX(mets) ? $rluiten$stemmer$Stemmer$step1b2X(mets) : drow;
			} else {
				return drow;
			}
		}
	}
};
var $rluiten$stemmer$Stemmer$step1cX = function (drow) {
	var _v0 = $elm$core$String$uncons(drow);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var c = _v1.a;
		var drowTail = _v1.b;
		return ((c === 'y') && $rluiten$stemmer$Stemmer$hasVowelX(drowTail)) ? A2($elm$core$String$cons, 'i', drowTail) : drow;
	} else {
		return drow;
	}
};
var $rluiten$stemmer$Stemmer$step1X = A2(
	$elm$core$Basics$composeL,
	A2($elm$core$Basics$composeL, $rluiten$stemmer$Stemmer$step1cX, $rluiten$stemmer$Stemmer$step1bX),
	$rluiten$stemmer$Stemmer$step1aX);
var $elm$core$String$append = _String_append;
var $rluiten$stemmer$Stemmer$replaceStartX = F3(
	function (measureThreshold, _v0, drow) {
		var start = _v0.a;
		var newStart = _v0.b;
		var startLen = $elm$core$String$length(start);
		var drowStart = A2($elm$core$String$left, startLen, drow);
		if (_Utils_eq(drowStart, start)) {
			var drowEnd = A2($elm$core$String$dropLeft, startLen, drow);
			return (_Utils_cmp(
				$rluiten$stemmer$Stemmer$measureX(drowEnd),
				measureThreshold) > 0) ? _Utils_Tuple2(
				true,
				A2($elm$core$String$append, newStart, drowEnd)) : _Utils_Tuple2(true, drow);
		} else {
			return _Utils_Tuple2(false, drow);
		}
	});
var $rluiten$stemmer$Stemmer$replaceStartsX = F3(
	function (measureThreshold, rules, drow) {
		replaceStartsX:
		while (true) {
			if (rules.b) {
				var r = rules.a;
				var rs = rules.b;
				var _v1 = A3($rluiten$stemmer$Stemmer$replaceStartX, measureThreshold, r, drow);
				var patternMatched = _v1.a;
				var newDrow = _v1.b;
				if (patternMatched) {
					return newDrow;
				} else {
					var $temp$measureThreshold = measureThreshold,
						$temp$rules = rs,
						$temp$drow = drow;
					measureThreshold = $temp$measureThreshold;
					rules = $temp$rules;
					drow = $temp$drow;
					continue replaceStartsX;
				}
			} else {
				return drow;
			}
		}
	});
var $rluiten$stemmer$Stemmer$toR = $elm$core$String$reverse;
var $rluiten$stemmer$Stemmer$step2RulesX = _List_fromArray(
	[
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ational'),
		$rluiten$stemmer$Stemmer$toR('ate')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('tional'),
		$rluiten$stemmer$Stemmer$toR('tion')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('enci'),
		$rluiten$stemmer$Stemmer$toR('ence')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('anci'),
		$rluiten$stemmer$Stemmer$toR('ance')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('izer'),
		$rluiten$stemmer$Stemmer$toR('ize')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('bli'),
		$rluiten$stemmer$Stemmer$toR('ble')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('alli'),
		$rluiten$stemmer$Stemmer$toR('al')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('entli'),
		$rluiten$stemmer$Stemmer$toR('ent')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('eli'),
		$rluiten$stemmer$Stemmer$toR('e')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ousli'),
		$rluiten$stemmer$Stemmer$toR('ous')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ization'),
		$rluiten$stemmer$Stemmer$toR('ize')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ation'),
		$rluiten$stemmer$Stemmer$toR('ate')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ator'),
		$rluiten$stemmer$Stemmer$toR('ate')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('alism'),
		$rluiten$stemmer$Stemmer$toR('al')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('iveness'),
		$rluiten$stemmer$Stemmer$toR('ive')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('fulness'),
		$rluiten$stemmer$Stemmer$toR('ful')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ousness'),
		$rluiten$stemmer$Stemmer$toR('ous')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('aliti'),
		$rluiten$stemmer$Stemmer$toR('al')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('iviti'),
		$rluiten$stemmer$Stemmer$toR('ive')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('biliti'),
		$rluiten$stemmer$Stemmer$toR('ble')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('logi'),
		$rluiten$stemmer$Stemmer$toR('log'))
	]);
var $rluiten$stemmer$Stemmer$step2X = function (drow) {
	return A3($rluiten$stemmer$Stemmer$replaceStartsX, 0, $rluiten$stemmer$Stemmer$step2RulesX, drow);
};
var $rluiten$stemmer$Stemmer$step3RulesX = _List_fromArray(
	[
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('icate'),
		$rluiten$stemmer$Stemmer$toR('ic')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ative'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('alize'),
		$rluiten$stemmer$Stemmer$toR('al')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('iciti'),
		$rluiten$stemmer$Stemmer$toR('ic')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ical'),
		$rluiten$stemmer$Stemmer$toR('ic')),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ful'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ness'),
		'')
	]);
var $rluiten$stemmer$Stemmer$step3X = function (drow) {
	return A3($rluiten$stemmer$Stemmer$replaceStartsX, 0, $rluiten$stemmer$Stemmer$step3RulesX, drow);
};
var $rluiten$stemmer$Stemmer$step4IonX = F3(
	function (mThreshold, startLen, drow) {
		var afterNoi = A2($elm$core$String$dropLeft, startLen, drow);
		var _v0 = $elm$core$String$uncons(afterNoi);
		if (!_v0.$) {
			var _v1 = _v0.a;
			var _char = _v1.a;
			var drowEnd = _v1.b;
			return (((_char === 't') || (_char === 's')) && (_Utils_cmp(
				$rluiten$stemmer$Stemmer$measureX(afterNoi),
				mThreshold) > 0)) ? afterNoi : drow;
		} else {
			return drow;
		}
	});
var $rluiten$stemmer$Stemmer$step4RulesX = _List_fromArray(
	[
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('al'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ance'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ence'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('er'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ic'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('able'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ible'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ant'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ement'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ment'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ent'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ou'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ism'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ate'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('iti'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ous'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ive'),
		''),
		_Utils_Tuple2(
		$rluiten$stemmer$Stemmer$toR('ize'),
		'')
	]);
var $rluiten$stemmer$Stemmer$step4X = function (drow) {
	var mThreshold = 1;
	var ionCase = 'noi';
	var ionLen = $elm$core$String$length(ionCase);
	var drowStart = A2($elm$core$String$left, ionLen, drow);
	return _Utils_eq(drowStart, ionCase) ? A3($rluiten$stemmer$Stemmer$step4IonX, mThreshold, ionLen, drow) : A3($rluiten$stemmer$Stemmer$replaceStartsX, mThreshold, $rluiten$stemmer$Stemmer$step4RulesX, drow);
};
var $rluiten$stemmer$Stemmer$step5aX = function (drow) {
	var _v0 = $elm$core$String$uncons(drow);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var _char = _v1.a;
		var drowEnd = _v1.b;
		if (_char === 'e') {
			var m = $rluiten$stemmer$Stemmer$measureX(drowEnd);
			return (m > 1) ? drowEnd : (((m === 1) && (!$rluiten$stemmer$Stemmer$endsWithCVCX(drowEnd))) ? drowEnd : drow);
		} else {
			return drow;
		}
	} else {
		return drow;
	}
};
var $rluiten$stemmer$Stemmer$step5bX = function (drow) {
	var _v0 = $elm$core$String$uncons(drow);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var _char = _v1.a;
		var drowEnd = _v1.b;
		return ((_char === 'l') && (($rluiten$stemmer$Stemmer$measureX(drowEnd) > 1) && $rluiten$stemmer$Stemmer$endsWithDoubleConsX(drow))) ? drowEnd : drow;
	} else {
		return drow;
	}
};
var $rluiten$stemmer$Stemmer$step5X = A2($elm$core$Basics$composeL, $rluiten$stemmer$Stemmer$step5bX, $rluiten$stemmer$Stemmer$step5aX);
var $rluiten$stemmer$Stemmer$allStepsX = A2(
	$elm$core$Basics$composeL,
	A2(
		$elm$core$Basics$composeL,
		A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				A2(
					$elm$core$Basics$composeL,
					A2($elm$core$Basics$composeL, $elm$core$String$reverse, $rluiten$stemmer$Stemmer$step5X),
					$rluiten$stemmer$Stemmer$step4X),
				$rluiten$stemmer$Stemmer$step3X),
			$rluiten$stemmer$Stemmer$step2X),
		$rluiten$stemmer$Stemmer$step1X),
	$elm$core$String$reverse);
var $rluiten$stemmer$Stemmer$stem = function (word) {
	return ($elm$core$String$length(word) < 3) ? word : $rluiten$stemmer$Stemmer$allStepsX(word);
};
var $rluiten$elm_text_search$Index$Defaults$defaultStemmerFuncCreator = $rluiten$elm_text_search$Index$Utils$createFuncCreator($rluiten$stemmer$Stemmer$stem);
var $rluiten$elm_text_search$Index$Defaults$defaultTransformFactories = _List_fromArray(
	[$rluiten$elm_text_search$Index$Defaults$defaultStemmerFuncCreator]);
var $rluiten$elm_text_search$Index$Defaults$getDefaultIndexConfig = function (_v0) {
	var indexType = _v0.K;
	var ref = _v0.bf;
	var fields = _v0.aY;
	var listFields = _v0.a4;
	return {aY: fields, ab: $rluiten$elm_text_search$Index$Defaults$defaultFilterFactories, K: indexType, ag: $rluiten$elm_text_search$Index$Defaults$defaultInitialTransformFactories, a4: listFields, bf: ref, aD: $rluiten$elm_text_search$Index$Defaults$defaultTransformFactories};
};
var $rluiten$trie$TrieModel$empty = $rluiten$trie$TrieModel$EmptyTrie;
var $rluiten$trie$Trie$empty = $rluiten$trie$TrieModel$empty;
var $rluiten$elm_text_search$Index$Defaults$indexVersion = '1.1.0';
var $rluiten$elm_text_search$Index$newWith = function (_v0) {
	var indexType = _v0.K;
	var ref = _v0.bf;
	var fields = _v0.aY;
	var listFields = _v0.a4;
	var initialTransformFactories = _v0.ag;
	var transformFactories = _v0.aD;
	var filterFactories = _v0.ab;
	return {G: $elm$core$Set$empty, Y: $elm$core$Dict$empty, H: $elm$core$Dict$empty, aY: fields, ab: filterFactories, aZ: $elm$core$Maybe$Nothing, J: $elm$core$Dict$empty, K: indexType, L: $rluiten$elm_text_search$Index$Defaults$indexVersion, ag: initialTransformFactories, a3: $elm$core$Maybe$Nothing, a4: listFields, bf: ref, T: $rluiten$trie$Trie$empty, aD: transformFactories, bs: $elm$core$Maybe$Nothing};
};
var $rluiten$elm_text_search$Index$new = function (simpleConfig) {
	return $rluiten$elm_text_search$Index$newWith(
		$rluiten$elm_text_search$Index$Defaults$getDefaultIndexConfig(simpleConfig));
};
var $rluiten$elm_text_search$ElmTextSearch$new = function (simpleConfig) {
	return $rluiten$elm_text_search$Index$new(
		$rluiten$elm_text_search$Index$Defaults$getIndexSimpleConfig(simpleConfig));
};
var $author$project$Update$initIndex = $rluiten$elm_text_search$ElmTextSearch$new(
	{
		aY: _List_fromArray(
			[
				_Utils_Tuple2(
				function ($) {
					return $.N;
				},
				1.0)
			]),
		a4: _List_Nil,
		bf: function ($) {
			return $.N;
		}
	});
var $author$project$Update$makeSearchIndex = function (codes) {
	return A2(
		$rluiten$elm_text_search$ElmTextSearch$addDocs,
		$author$project$Update$indexContent(codes),
		$author$project$Update$initIndex).a;
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $author$project$Data$overallUniCodes = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(1, 'ACM Guildford Limited'),
			_Utils_Tuple2(2, 'Abertay University'),
			_Utils_Tuple2(3, 'Aberystwyth University'),
			_Utils_Tuple2(4, 'Anglia Ruskin University Higher Education Corporation'),
			_Utils_Tuple2(5, 'Arden University Limited'),
			_Utils_Tuple2(6, 'Arts University Bournemouth, the'),
			_Utils_Tuple2(7, 'Aston University'),
			_Utils_Tuple2(8, 'BIMM Limited'),
			_Utils_Tuple2(9, 'Bangor University'),
			_Utils_Tuple2(10, 'Bath Spa University'),
			_Utils_Tuple2(11, 'Birkbeck College'),
			_Utils_Tuple2(12, 'Birmingham City University'),
			_Utils_Tuple2(13, 'Bishop Grosseteste University'),
			_Utils_Tuple2(14, 'Blackpool and the Fylde College'),
			_Utils_Tuple2(15, 'Bournemouth University'),
			_Utils_Tuple2(16, 'Bradford College'),
			_Utils_Tuple2(17, 'Brunel University London'),
			_Utils_Tuple2(18, 'Buckinghamshire New University'),
			_Utils_Tuple2(19, 'Canterbury Christ Church University'),
			_Utils_Tuple2(20, 'Cardiff Metropolitan University'),
			_Utils_Tuple2(21, 'Cardiff University'),
			_Utils_Tuple2(22, 'City, University of London'),
			_Utils_Tuple2(23, 'Coventry University'),
			_Utils_Tuple2(24, 'DN Colleges Group'),
			_Utils_Tuple2(25, 'De Montfort University'),
			_Utils_Tuple2(26, 'Edge Hill University'),
			_Utils_Tuple2(27, 'Edinburgh Napier University'),
			_Utils_Tuple2(28, 'Falmouth University'),
			_Utils_Tuple2(29, 'Glasgow Caledonian University'),
			_Utils_Tuple2(30, 'Glasgow School of Art.'),
			_Utils_Tuple2(31, 'Goldsmiths\' College'),
			_Utils_Tuple2(32, 'Harper Adams University'),
			_Utils_Tuple2(33, 'Hartpury University'),
			_Utils_Tuple2(34, 'Heriot-Watt University'),
			_Utils_Tuple2(35, 'ICMP Management Limited'),
			_Utils_Tuple2(36, 'Imperial College of Science, Technology and Medicine'),
			_Utils_Tuple2(37, 'King\'s College London'),
			_Utils_Tuple2(38, 'Kingston University'),
			_Utils_Tuple2(39, 'LTE Group'),
			_Utils_Tuple2(40, 'Leeds Arts University'),
			_Utils_Tuple2(41, 'Leeds Beckett University'),
			_Utils_Tuple2(42, 'Leeds College of Music'),
			_Utils_Tuple2(43, 'Leeds Trinity University'),
			_Utils_Tuple2(44, 'Liverpool Hope University'),
			_Utils_Tuple2(45, 'Liverpool John Moores University'),
			_Utils_Tuple2(46, 'London Metropolitan University'),
			_Utils_Tuple2(47, 'London South Bank University'),
			_Utils_Tuple2(48, 'Loughborough College'),
			_Utils_Tuple2(49, 'Loughborough University'),
			_Utils_Tuple2(50, 'Manchester Metropolitan University'),
			_Utils_Tuple2(51, 'Medway School of Pharmacy'),
			_Utils_Tuple2(52, 'Middlesex University'),
			_Utils_Tuple2(53, 'Newman University'),
			_Utils_Tuple2(54, 'Norwich University of the Arts'),
			_Utils_Tuple2(55, 'Nottingham Trent University'),
			_Utils_Tuple2(56, 'Oxford Brookes University'),
			_Utils_Tuple2(57, 'Pearson College Limited'),
			_Utils_Tuple2(58, 'Plymouth College of Art'),
			_Utils_Tuple2(59, 'QAHE (UR) Limited'),
			_Utils_Tuple2(60, 'Queen Margaret University, Edinburgh'),
			_Utils_Tuple2(61, 'Queen Mary University of London'),
			_Utils_Tuple2(62, 'Queen\'s University of Belfast'),
			_Utils_Tuple2(63, 'Ravensbourne University London'),
			_Utils_Tuple2(64, 'Regent\'s University London'),
			_Utils_Tuple2(65, 'Robert Gordon University'),
			_Utils_Tuple2(66, 'Roehampton University'),
			_Utils_Tuple2(67, 'Royal Conservatoire of Scotland'),
			_Utils_Tuple2(68, 'Royal Holloway and Bedford New College'),
			_Utils_Tuple2(69, 'SAE Education Limited'),
			_Utils_Tuple2(70, 'Sheffield Hallam University'),
			_Utils_Tuple2(71, 'Solent University'),
			_Utils_Tuple2(72, 'St Mary\'s University College'),
			_Utils_Tuple2(73, 'St Mary\'s University, Twickenham'),
			_Utils_Tuple2(74, 'St. George\'s Hospital Medical School'),
			_Utils_Tuple2(75, 'Staffordshire University'),
			_Utils_Tuple2(76, 'Stranmillis University College'),
			_Utils_Tuple2(77, 'Swansea University'),
			_Utils_Tuple2(78, 'Teesside University'),
			_Utils_Tuple2(79, 'The Conservatoire for Dance and Drama'),
			_Utils_Tuple2(80, 'The Liverpool Institute for Performing Arts'),
			_Utils_Tuple2(81, 'The London School of Economics and Political Science'),
			_Utils_Tuple2(82, 'The Open University'),
			_Utils_Tuple2(83, 'The Royal Agricultural University'),
			_Utils_Tuple2(84, 'The Royal Veterinary College'),
			_Utils_Tuple2(85, 'The School of Oriental and African Studies'),
			_Utils_Tuple2(86, 'The University of Bath'),
			_Utils_Tuple2(87, 'The University of Birmingham'),
			_Utils_Tuple2(88, 'The University of Bolton'),
			_Utils_Tuple2(89, 'The University of Bradford'),
			_Utils_Tuple2(90, 'The University of Buckingham'),
			_Utils_Tuple2(91, 'The University of Chichester'),
			_Utils_Tuple2(92, 'The University of Cumbria'),
			_Utils_Tuple2(93, 'The University of East Anglia'),
			_Utils_Tuple2(94, 'The University of Essex'),
			_Utils_Tuple2(95, 'The University of Huddersfield'),
			_Utils_Tuple2(96, 'The University of Hull'),
			_Utils_Tuple2(97, 'The University of Kent'),
			_Utils_Tuple2(98, 'The University of Lancaster'),
			_Utils_Tuple2(99, 'The University of Law Limited'),
			_Utils_Tuple2(100, 'The University of Leeds'),
			_Utils_Tuple2(101, 'The University of Leicester'),
			_Utils_Tuple2(102, 'The University of Liverpool'),
			_Utils_Tuple2(103, 'The University of Manchester'),
			_Utils_Tuple2(104, 'The University of Reading'),
			_Utils_Tuple2(105, 'The University of Sheffield'),
			_Utils_Tuple2(106, 'The University of Surrey'),
			_Utils_Tuple2(107, 'The University of Warwick'),
			_Utils_Tuple2(108, 'The University of West London'),
			_Utils_Tuple2(109, 'The University of Westminster'),
			_Utils_Tuple2(110, 'Trinity Laban Conservatoire of Music and Dance'),
			_Utils_Tuple2(111, 'UCFB College of Football Business Limited'),
			_Utils_Tuple2(112, 'University College Birmingham'),
			_Utils_Tuple2(113, 'University College London'),
			_Utils_Tuple2(114, 'University College of Estate Management'),
			_Utils_Tuple2(115, 'University for the Creative Arts'),
			_Utils_Tuple2(116, 'University of Aberdeen'),
			_Utils_Tuple2(117, 'University of Bedfordshire'),
			_Utils_Tuple2(118, 'University of Brighton'),
			_Utils_Tuple2(119, 'University of Bristol'),
			_Utils_Tuple2(120, 'University of Central Lancashire'),
			_Utils_Tuple2(121, 'University of Chester'),
			_Utils_Tuple2(122, 'University of Derby'),
			_Utils_Tuple2(123, 'University of Dundee'),
			_Utils_Tuple2(124, 'University of Durham'),
			_Utils_Tuple2(125, 'University of East London'),
			_Utils_Tuple2(126, 'University of Edinburgh'),
			_Utils_Tuple2(127, 'University of Exeter'),
			_Utils_Tuple2(128, 'University of Glasgow'),
			_Utils_Tuple2(129, 'University of Gloucestershire'),
			_Utils_Tuple2(130, 'University of Greenwich'),
			_Utils_Tuple2(131, 'University of Hertfordshire'),
			_Utils_Tuple2(132, 'University of Keele'),
			_Utils_Tuple2(133, 'University of Lincoln'),
			_Utils_Tuple2(134, 'University of Newcastle upon Tyne'),
			_Utils_Tuple2(135, 'University of Northampton, The'),
			_Utils_Tuple2(136, 'University of Northumbria at Newcastle'),
			_Utils_Tuple2(137, 'University of Nottingham, The'),
			_Utils_Tuple2(138, 'University of Plymouth'),
			_Utils_Tuple2(139, 'University of Portsmouth'),
			_Utils_Tuple2(140, 'University of Salford, The'),
			_Utils_Tuple2(141, 'University of South Wales'),
			_Utils_Tuple2(142, 'University of Southampton'),
			_Utils_Tuple2(143, 'University of St Andrews'),
			_Utils_Tuple2(144, 'University of St Mark & St John'),
			_Utils_Tuple2(145, 'University of Stirling'),
			_Utils_Tuple2(146, 'University of Strathclyde'),
			_Utils_Tuple2(147, 'University of Suffolk'),
			_Utils_Tuple2(148, 'University of Sunderland'),
			_Utils_Tuple2(149, 'University of Sussex'),
			_Utils_Tuple2(150, 'University of Ulster'),
			_Utils_Tuple2(151, 'University of Wales Trinity Saint David'),
			_Utils_Tuple2(152, 'University of Winchester'),
			_Utils_Tuple2(153, 'University of Wolverhampton'),
			_Utils_Tuple2(154, 'University of Worcester'),
			_Utils_Tuple2(155, 'University of York'),
			_Utils_Tuple2(156, 'University of the Arts, London'),
			_Utils_Tuple2(157, 'University of the Highlands and Islands'),
			_Utils_Tuple2(158, 'University of the West of England, Bristol'),
			_Utils_Tuple2(159, 'University of the West of Scotland'),
			_Utils_Tuple2(160, 'Warwickshire College'),
			_Utils_Tuple2(161, 'Wrexham Glyndŵr University'),
			_Utils_Tuple2(162, 'York St John University')
		]));
var $author$project$Data$subjectCodes = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(1, 'Accounting'),
			_Utils_Tuple2(2, 'Adult nursing'),
			_Utils_Tuple2(3, 'Aeronautical and aerospace engineering'),
			_Utils_Tuple2(4, 'African and modern Middle Eastern studies'),
			_Utils_Tuple2(5, 'Agriculture'),
			_Utils_Tuple2(6, 'American and Australasian studies'),
			_Utils_Tuple2(7, 'Anatomy, physiology and pathology'),
			_Utils_Tuple2(8, 'Animal science'),
			_Utils_Tuple2(9, 'Anthropology'),
			_Utils_Tuple2(10, 'Applied psychology'),
			_Utils_Tuple2(11, 'Archaeology'),
			_Utils_Tuple2(12, 'Architecture'),
			_Utils_Tuple2(13, 'Art'),
			_Utils_Tuple2(14, 'Artificial intelligence'),
			_Utils_Tuple2(15, 'Asian studies'),
			_Utils_Tuple2(16, 'Astronomy'),
			_Utils_Tuple2(17, 'Bioengineering, medical and biomedical engineering'),
			_Utils_Tuple2(18, 'Biology (non-specific)'),
			_Utils_Tuple2(19, 'Biomedical sciences (non-specific)'),
			_Utils_Tuple2(20, 'Biosciences (non-specific)'),
			_Utils_Tuple2(21, 'Building'),
			_Utils_Tuple2(22, 'Business and management (non-specific)'),
			_Utils_Tuple2(23, 'Business studies'),
			_Utils_Tuple2(24, 'Chemical, process and energy engineering'),
			_Utils_Tuple2(25, 'Chemistry'),
			_Utils_Tuple2(26, 'Childhood and youth studies'),
			_Utils_Tuple2(27, 'Children\'s nursing'),
			_Utils_Tuple2(28, 'Cinematics and photography'),
			_Utils_Tuple2(29, 'Civil engineering'),
			_Utils_Tuple2(30, 'Classics'),
			_Utils_Tuple2(31, 'Combined, general or negotiated studies'),
			_Utils_Tuple2(32, 'Complementary and alternative medicine'),
			_Utils_Tuple2(33, 'Computer games and animation'),
			_Utils_Tuple2(34, 'Computer science'),
			_Utils_Tuple2(35, 'Counselling, psychotherapy and occupational therapy'),
			_Utils_Tuple2(36, 'Creative writing'),
			_Utils_Tuple2(37, 'Dance'),
			_Utils_Tuple2(38, 'Dentistry'),
			_Utils_Tuple2(39, 'Design studies'),
			_Utils_Tuple2(40, 'Development studies'),
			_Utils_Tuple2(41, 'Developmental psychology'),
			_Utils_Tuple2(42, 'Drama'),
			_Utils_Tuple2(43, 'Earth sciences'),
			_Utils_Tuple2(44, 'Ecology and environmental biology'),
			_Utils_Tuple2(45, 'Economics'),
			_Utils_Tuple2(46, 'Education'),
			_Utils_Tuple2(47, 'Electrical and electronic engineering'),
			_Utils_Tuple2(48, 'Engineering (non-specific)'),
			_Utils_Tuple2(49, 'English language'),
			_Utils_Tuple2(50, 'English studies (non-specific)'),
			_Utils_Tuple2(51, 'Environmental and public health'),
			_Utils_Tuple2(52, 'Environmental sciences'),
			_Utils_Tuple2(53, 'Finance'),
			_Utils_Tuple2(54, 'Food and beverage production'),
			_Utils_Tuple2(55, 'Food and beverage studies (non-specific)'),
			_Utils_Tuple2(56, 'Food sciences'),
			_Utils_Tuple2(57, 'Forensic and archaeological sciences'),
			_Utils_Tuple2(58, 'French studies'),
			_Utils_Tuple2(59, 'Genetics'),
			_Utils_Tuple2(60, 'German and Scandinavian studies'),
			_Utils_Tuple2(61, 'Health sciences (non-specific)'),
			_Utils_Tuple2(62, 'Health studies'),
			_Utils_Tuple2(63, 'Healthcare science (non-specific)'),
			_Utils_Tuple2(64, 'History'),
			_Utils_Tuple2(65, 'History of art, architecture and design'),
			_Utils_Tuple2(66, 'Human geography'),
			_Utils_Tuple2(67, 'Human resource management'),
			_Utils_Tuple2(68, 'Humanities (non-specific)'),
			_Utils_Tuple2(69, 'Iberian studies'),
			_Utils_Tuple2(70, 'Information services'),
			_Utils_Tuple2(71, 'Information systems'),
			_Utils_Tuple2(72, 'Italian studies'),
			_Utils_Tuple2(73, 'Journalism'),
			_Utils_Tuple2(74, 'Landscape design'),
			_Utils_Tuple2(75, 'Law'),
			_Utils_Tuple2(76, 'Learning disabilities nursing'),
			_Utils_Tuple2(77, 'Linguistics'),
			_Utils_Tuple2(78, 'Literature in English'),
			_Utils_Tuple2(79, 'Management studies'),
			_Utils_Tuple2(80, 'Maritime technology'),
			_Utils_Tuple2(81, 'Marketing'),
			_Utils_Tuple2(82, 'Materials science'),
			_Utils_Tuple2(83, 'Materials technology'),
			_Utils_Tuple2(84, 'Mathematics'),
			_Utils_Tuple2(85, 'Mechanical engineering'),
			_Utils_Tuple2(86, 'Media studies'),
			_Utils_Tuple2(87, 'Medical technology'),
			_Utils_Tuple2(88, 'Medicine (non-specific)'),
			_Utils_Tuple2(89, 'Mental health nursing'),
			_Utils_Tuple2(90, 'Microbiology and cell science'),
			_Utils_Tuple2(91, 'Midwifery'),
			_Utils_Tuple2(92, 'Molecular biology, biophysics and biochemistry'),
			_Utils_Tuple2(93, 'Music'),
			_Utils_Tuple2(94, 'Naval architecture'),
			_Utils_Tuple2(95, 'Nursing (non-specific)'),
			_Utils_Tuple2(96, 'Nutrition and dietetics'),
			_Utils_Tuple2(97, 'Operational research'),
			_Utils_Tuple2(98, 'Ophthalmics'),
			_Utils_Tuple2(99, 'Others in Celtic studies'),
			_Utils_Tuple2(100, 'Others in biosciences'),
			_Utils_Tuple2(101, 'Others in business and management'),
			_Utils_Tuple2(102, 'Others in computing'),
			_Utils_Tuple2(103, 'Others in creative arts and design'),
			_Utils_Tuple2(104, 'Others in engineering'),
			_Utils_Tuple2(105, 'Others in language and area studies'),
			_Utils_Tuple2(106, 'Others in nursing'),
			_Utils_Tuple2(107, 'Others in psychology'),
			_Utils_Tuple2(108, 'Others in technology'),
			_Utils_Tuple2(109, 'Others in veterinary sciences'),
			_Utils_Tuple2(110, 'Pharmacology'),
			_Utils_Tuple2(111, 'Pharmacy'),
			_Utils_Tuple2(112, 'Philosophy'),
			_Utils_Tuple2(113, 'Physical geographical sciences'),
			_Utils_Tuple2(114, 'Physical sciences (non-specific)'),
			_Utils_Tuple2(115, 'Physics'),
			_Utils_Tuple2(116, 'Physiotherapy'),
			_Utils_Tuple2(117, 'Planning (urban, rural and regional)'),
			_Utils_Tuple2(118, 'Politics'),
			_Utils_Tuple2(119, 'Polymers and textiles'),
			_Utils_Tuple2(120, 'Production and manufacturing engineering'),
			_Utils_Tuple2(121, 'Psychology (non-specific)'),
			_Utils_Tuple2(122, 'Psychology and health'),
			_Utils_Tuple2(123, 'Publicity studies'),
			_Utils_Tuple2(124, 'Rural estate management'),
			_Utils_Tuple2(125, 'Slavic studies'),
			_Utils_Tuple2(126, 'Social policy'),
			_Utils_Tuple2(127, 'Social sciences (non-specific)'),
			_Utils_Tuple2(128, 'Social work'),
			_Utils_Tuple2(129, 'Sociology'),
			_Utils_Tuple2(130, 'Software engineering'),
			_Utils_Tuple2(131, 'Sport and exercise sciences'),
			_Utils_Tuple2(132, 'Statistics'),
			_Utils_Tuple2(133, 'Teacher training'),
			_Utils_Tuple2(134, 'Theology and religious studies'),
			_Utils_Tuple2(135, 'Tourism, transport and travel'),
			_Utils_Tuple2(136, 'Veterinary medicine and dentistry'),
			_Utils_Tuple2(137, 'Zoology')
		]));
var $author$project$Main$initModel = {
	aN: 0,
	X: $author$project$DataTypes$Overall,
	aR: $author$project$DataTypes$FoldedUp,
	aS: _List_Nil,
	aX: $elm$core$Maybe$Nothing,
	a_: $elm$core$Maybe$Nothing,
	bc: false,
	bd: _List_fromArray(
		[27]),
	bm: $author$project$Update$makeSearchIndex($author$project$Data$subjectCodes),
	bn: _List_Nil,
	bt: $author$project$Update$makeSearchIndex($author$project$Data$overallUniCodes),
	bu: _List_Nil,
	bv: $elm$core$Dict$keys($author$project$Data$overallUniCodes)
};
var $author$project$Main$init = function (_v0) {
	return $author$project$Update$freshModel($author$project$Main$initModel);
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$DataTypes$BySubject = function (a) {
	return {$: 1, a: a};
};
var $author$project$DataTypes$QVsA = 1;
var $author$project$DataTypes$Question = {$: 0};
var $author$project$DataTypes$Subject = function (a) {
	return {$: 1, a: a};
};
var $author$project$DataTypes$Uni = function (a) {
	return {$: 2, a: a};
};
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $author$project$Update$calculateDataChoice = F2(
	function (currentChoices, _new) {
		return A2($elm$core$List$member, _new, currentChoices) ? A2(
			$elm$core$List$filter,
			function (x) {
				return !_Utils_eq(x, _new);
			},
			currentChoices) : $elm$core$List$sort(
			A2($elm$core$List$cons, _new, currentChoices));
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 1) {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $elm_community$maybe_extra$Maybe$Extra$combine = A2(
	$elm$core$List$foldr,
	$elm$core$Maybe$map2($elm$core$List$cons),
	$elm$core$Maybe$Just(_List_Nil));
var $author$project$Update$oneDat2Tuple = function (dat) {
	if (((dat.b && dat.b.b) && dat.b.b.b) && (!dat.b.b.b.b)) {
		var a = dat.a;
		var _v1 = dat.b;
		var b = _v1.a;
		var _v2 = _v1.b;
		var c = _v2.a;
		return $elm$core$Maybe$Just(
			_Utils_Tuple3(a, b, c));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Update$data2Tuple = function (dat) {
	return $elm_community$maybe_extra$Maybe$Extra$combine(
		A2($elm$core$List$map, $author$project$Update$oneDat2Tuple, dat));
};
var $author$project$Update$errToStr = function (e) {
	switch (e.$) {
		case 0:
			var str = e.a;
			return 'bad url: ' + str;
		case 1:
			return 'timeout';
		case 2:
			return 'network error';
		case 3:
			var s = e.a;
			return 'bad status: ' + $elm$core$String$fromInt(s);
		default:
			var str = e.a;
			return 'bad body: ' + str;
	}
};
var $author$project$DataTypes$questionCodes = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(1, 'Staff are good at explaining things.'),
			_Utils_Tuple2(2, 'Staff have made the subject interesting.'),
			_Utils_Tuple2(3, 'The course is intellectually stimulating.'),
			_Utils_Tuple2(4, 'My course has challenged me to achieve my best work.'),
			_Utils_Tuple2(5, 'My course has provided me with opportunities to explore ideas or concepts in depth.'),
			_Utils_Tuple2(6, 'My course has provided me with opportunities to bring information and ideas together from different topics.'),
			_Utils_Tuple2(7, 'My course has provided me with opportunities to apply what I have learnt.'),
			_Utils_Tuple2(8, 'The criteria used in marking have been clear in advance.'),
			_Utils_Tuple2(9, 'Marking and assessment has been fair.'),
			_Utils_Tuple2(10, 'Feedback on my work has been timely.'),
			_Utils_Tuple2(11, 'I have received helpful comments on my work.'),
			_Utils_Tuple2(12, 'I have been able to contact staff when I needed to.'),
			_Utils_Tuple2(13, 'I have received sufficient advice and guidance in relation to my course.'),
			_Utils_Tuple2(14, 'Good advice was available when I needed to make study choices on my course.'),
			_Utils_Tuple2(15, 'The course is well organised and running smoothly.'),
			_Utils_Tuple2(16, 'The timetable works efficiently for me.'),
			_Utils_Tuple2(17, 'Any changes in the course or teaching have been communicated effectively.'),
			_Utils_Tuple2(18, 'The IT resources and facilities provided have supported my learning well.'),
			_Utils_Tuple2(19, 'The library resources (e.g. books, online services and learning spaces) have supported my learning well.'),
			_Utils_Tuple2(20, 'I have been able to access course-specific resources (e.g. equipment, facilities, software, collections) when I needed to.'),
			_Utils_Tuple2(21, 'I feel part of a community of staff and students.'),
			_Utils_Tuple2(22, 'I have had the right opportunities to work with other students as part of my course.'),
			_Utils_Tuple2(23, 'I have had the right opportunities to provide feedback on my course.'),
			_Utils_Tuple2(24, 'Staff value students\' views and opinions about the course.'),
			_Utils_Tuple2(25, 'It is clear how students\' feedback on the course has been acted on.'),
			_Utils_Tuple2(26, 'The students\' union (association or guild) effectively represents students\' academic interests.'),
			_Utils_Tuple2(27, 'Overall, I am satisfied with the quality of the course.')
		]));
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $rluiten$trie$TrieModel$getNodeCore = F2(
	function (key, trie) {
		if (!key.b) {
			return $elm$core$Maybe$Just(trie);
		} else {
			var keyHead = key.a;
			var keyTail = key.b;
			var getTrie = function (trieDict) {
				return A2(
					$elm$core$Maybe$andThen,
					$rluiten$trie$TrieModel$getNodeCore(keyTail),
					A2($elm$core$Dict$get, keyHead, trieDict));
			};
			switch (trie.$) {
				case 0:
					return $elm$core$Maybe$Nothing;
				case 1:
					return $elm$core$Maybe$Nothing;
				case 2:
					var trieDict = trie.a;
					return getTrie(trieDict);
				default:
					var _v2 = trie.a;
					var trieDict = _v2.b;
					return getTrie(trieDict);
			}
		}
	});
var $rluiten$trie$TrieModel$getNodeByStr = F2(
	function (key, trie) {
		return $elm$core$List$isEmpty(key) ? $elm$core$Maybe$Nothing : A2($rluiten$trie$TrieModel$getNodeCore, key, trie);
	});
var $rluiten$trie$TrieModel$getNode = F2(
	function (key, trie) {
		return A2(
			$rluiten$trie$TrieModel$getNodeByStr,
			$rluiten$trie$TrieModel$toListString(key),
			trie);
	});
var $rluiten$trie$Trie$getNode = $rluiten$trie$TrieModel$getNode;
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $rluiten$trie$TrieModel$expandCore = F3(
	function (key, trie, keyList) {
		var expandSub = F3(
			function (_char, trie1, foldList) {
				return A3(
					$rluiten$trie$TrieModel$expandCore,
					_Utils_ap(
						key,
						_List_fromArray(
							[_char])),
					trie1,
					foldList);
			});
		var addRefKey = function (refValues) {
			return (!$elm$core$Dict$isEmpty(refValues)) ? A2(
				$elm$core$List$cons,
				$elm$core$String$concat(key),
				keyList) : keyList;
		};
		switch (trie.$) {
			case 0:
				return keyList;
			case 1:
				var refValues = trie.a;
				return addRefKey(refValues);
			case 2:
				var trieDict = trie.a;
				return A3($elm$core$Dict$foldr, expandSub, keyList, trieDict);
			default:
				var _v1 = trie.a;
				var refValues = _v1.a;
				var trieDict = _v1.b;
				var dirtyList = addRefKey(refValues);
				return A3($elm$core$Dict$foldr, expandSub, dirtyList, trieDict);
		}
	});
var $rluiten$trie$TrieModel$expandByStr = F2(
	function (key, trie) {
		var _v0 = A2($rluiten$trie$TrieModel$getNodeByStr, key, trie);
		if (_v0.$ === 1) {
			return _List_Nil;
		} else {
			var keyTrie = _v0.a;
			return A3($rluiten$trie$TrieModel$expandCore, key, keyTrie, _List_Nil);
		}
	});
var $rluiten$trie$TrieModel$expand = F2(
	function (key, trie) {
		return A2(
			$rluiten$trie$TrieModel$expandByStr,
			$rluiten$trie$TrieModel$toListString(key),
			trie);
	});
var $rluiten$trie$Trie$expand = $rluiten$trie$TrieModel$expand;
var $rluiten$trie$TrieModel$getValues = function (trie) {
	switch (trie.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var refValues = trie.a;
			return $elm$core$Maybe$Just(refValues);
		case 2:
			return $elm$core$Maybe$Nothing;
		default:
			var _v1 = trie.a;
			var refValues = _v1.a;
			return $elm$core$Maybe$Just(refValues);
	}
};
var $rluiten$trie$TrieModel$getByStr = F2(
	function (key, trie) {
		return A2(
			$elm$core$Maybe$andThen,
			$rluiten$trie$TrieModel$getValues,
			A2($rluiten$trie$TrieModel$getNodeByStr, key, trie));
	});
var $rluiten$trie$TrieModel$get = F2(
	function (key, trie) {
		return A2(
			$rluiten$trie$TrieModel$getByStr,
			$rluiten$trie$TrieModel$toListString(key),
			trie);
	});
var $rluiten$trie$Trie$get = $rluiten$trie$TrieModel$get;
var $rluiten$trie$TrieModel$valueCount = F2(
	function (key, trie) {
		return $elm$core$Dict$size(
			A2(
				$elm$core$Maybe$withDefault,
				$elm$core$Dict$empty,
				A2($rluiten$trie$TrieModel$get, key, trie)));
	});
var $rluiten$trie$Trie$valueCount = $rluiten$trie$TrieModel$valueCount;
var $rluiten$elm_text_search$Index$Utils$calcIdf = F2(
	function (_v0, token) {
		var irec = _v0;
		var docFrequency = A2($rluiten$trie$Trie$valueCount, token, irec.T);
		var idfLocal = (docFrequency > 0) ? (1 + A2(
			$elm$core$Basics$logBase,
			10,
			$elm$core$Dict$size(irec.H) / docFrequency)) : 1;
		var updatedIdfCache = A3($elm$core$Dict$insert, token, idfLocal, irec.J);
		var u1index = _Utils_update(
			irec,
			{J: updatedIdfCache});
		return _Utils_Tuple2(u1index, idfLocal);
	});
var $rluiten$elm_text_search$Index$Utils$idf = F2(
	function (index, token) {
		var irec = index;
		var _v0 = A2($elm$core$Dict$get, token, irec.J);
		if (_v0.$ === 1) {
			return A2($rluiten$elm_text_search$Index$Utils$calcIdf, index, token);
		} else {
			var idfValue = _v0.a;
			return _Utils_Tuple2(index, idfValue);
		}
	});
var $rluiten$sparsevector$SparseVector$insert = F3(
	function (index, value, svector) {
		return A3($elm$core$Dict$insert, index, value, svector);
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $rluiten$elm_text_search$Index$Vector$similarityBoost = F2(
	function (token, expandedToken) {
		return _Utils_eq(expandedToken, token) ? 1 : (1 / A2(
			$elm$core$Basics$logBase,
			10,
			A2(
				$elm$core$Basics$max,
				3,
				$elm$core$String$length(expandedToken) - $elm$core$String$length(token))));
	});
var $rluiten$elm_text_search$Index$Vector$updateSetAndVec = F4(
	function (tf, token, expandedToken, _v0) {
		var docSets = _v0.a;
		var vec = _v0.b;
		var index = _v0.c;
		var irec = index;
		var _v1 = A2($rluiten$elm_text_search$Index$Utils$idf, index, expandedToken);
		var u1index = _v1.a;
		var u1irec = u1index;
		var keyIdf = _v1.b;
		var tfidf = (tf * keyIdf) * A2($rluiten$elm_text_search$Index$Vector$similarityBoost, token, expandedToken);
		var u1vec = A2(
			$elm$core$Maybe$withDefault,
			vec,
			A2(
				$elm$core$Maybe$map,
				function (pos) {
					return A3($rluiten$sparsevector$SparseVector$insert, pos, tfidf, vec);
				},
				A2($elm$core$Dict$get, expandedToken, irec.Y)));
		var expandedTokenDocSet = A2(
			$elm$core$Maybe$withDefault,
			$elm$core$Set$empty,
			A2(
				$elm$core$Maybe$map,
				function (dict) {
					return $elm$core$Set$fromList(
						$elm$core$Dict$keys(dict));
				},
				A2($rluiten$trie$Trie$get, expandedToken, u1irec.T)));
		var u1docSets = A2($elm$core$Set$union, expandedTokenDocSet, docSets);
		return _Utils_Tuple3(u1docSets, u1vec, u1index);
	});
var $rluiten$elm_text_search$Index$Vector$buildDocVector = F4(
	function (tokensLength, fieldBoosts, baseToken, _v0) {
		var docSets = _v0.a;
		var vec = _v0.b;
		var index = _v0.c;
		var irec = index;
		var termFrequency = ((1 / tokensLength) * $elm$core$List$length(irec.aY)) * fieldBoosts;
		var expandedTokens = A2($rluiten$trie$Trie$expand, baseToken, irec.T);
		var _v1 = A3(
			$elm$core$List$foldr,
			A2($rluiten$elm_text_search$Index$Vector$updateSetAndVec, termFrequency, baseToken),
			_Utils_Tuple3($elm$core$Set$empty, vec, index),
			expandedTokens);
		var docs = _v1.a;
		var vecU1 = _v1.b;
		var indexU1 = _v1.c;
		return _Utils_Tuple3(
			A2($elm$core$List$cons, docs, docSets),
			vecU1,
			indexU1);
	});
var $rluiten$sparsevector$SparseVector$empty = $elm$core$Dict$empty;
var $rluiten$elm_text_search$Index$Vector$getQueryVector = F3(
	function (fieldBoosts, tokens, index) {
		return A3(
			$elm$core$List$foldr,
			A2(
				$rluiten$elm_text_search$Index$Vector$buildDocVector,
				$elm$core$List$length(tokens),
				fieldBoosts),
			_Utils_Tuple3(_List_Nil, $rluiten$sparsevector$SparseVector$empty, index),
			tokens);
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			$elm$core$Dict$filter,
			F2(
				function (k, _v0) {
					return A2($elm$core$Dict$member, k, t2);
				}),
			t1);
	});
var $elm$core$Set$intersect = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$intersect, dict1, dict2);
	});
var $rluiten$elm_text_search$Utils$intersectSets = function (sets) {
	if (!sets.b) {
		return $elm$core$Set$empty;
	} else {
		var h = sets.a;
		var tail = sets.b;
		return A3(
			$elm$core$List$foldr,
			F2(
				function (set, agg) {
					return A2($elm$core$Set$intersect, set, agg);
				}),
			h,
			tail);
	}
};
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $rluiten$sparsevector$SparseVector$dot = F2(
	function (vec1, vec2) {
		var common = A2(
			$elm$core$Set$intersect,
			$elm$core$Set$fromList(
				$elm$core$Dict$keys(vec1)),
			$elm$core$Set$fromList(
				$elm$core$Dict$keys(vec2)));
		var d1 = A2(
			$elm$core$Dict$filter,
			F2(
				function (k, v) {
					return A2($elm$core$Set$member, k, common);
				}),
			vec1);
		var d2 = A2(
			$elm$core$Dict$filter,
			F2(
				function (k, v) {
					return A2($elm$core$Set$member, k, common);
				}),
			vec2);
		return $elm$core$List$sum(
			A3(
				$elm$core$List$map2,
				F2(
					function (v1, v2) {
						return v1 * v2;
					}),
				$elm$core$Dict$values(d1),
				$elm$core$Dict$values(d2)));
	});
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $rluiten$sparsevector$SparseVector$magnitude = function (svector) {
	return $elm$core$Basics$sqrt(
		$elm$core$List$sum(
			A2(
				$elm$core$List$map,
				function (x) {
					return x * x;
				},
				$elm$core$Dict$values(svector))));
};
var $rluiten$sparsevector$SparseVector$cosineSimilarity = F2(
	function (vec1, vec2) {
		return A2($rluiten$sparsevector$SparseVector$dot, vec1, vec2) / ($rluiten$sparsevector$SparseVector$magnitude(vec1) * $rluiten$sparsevector$SparseVector$magnitude(vec2));
	});
var $rluiten$elm_text_search$Index$Vector$updateDocVector = F3(
	function (docRef, token, inputTuple) {
		var index = inputTuple.a;
		var irec = index;
		var docVector = inputTuple.b;
		return A2(
			$elm$core$Maybe$withDefault,
			inputTuple,
			A3(
				$elm$core$Maybe$map2,
				F2(
					function (position, termFrequency) {
						var _v0 = A2($rluiten$elm_text_search$Index$Utils$idf, index, token);
						var u1index = _v0.a;
						var idfScore = _v0.b;
						return _Utils_Tuple2(
							u1index,
							A3($rluiten$sparsevector$SparseVector$insert, position, termFrequency * idfScore, docVector));
					}),
				A2($elm$core$Dict$get, token, irec.Y),
				A2(
					$elm$core$Maybe$andThen,
					$elm$core$Dict$get(docRef),
					A2($rluiten$trie$Trie$get, token, irec.T))));
	});
var $rluiten$elm_text_search$Index$Vector$getDocVector = F2(
	function (index, docRef) {
		var irec = index;
		return A2(
			$elm$core$Maybe$withDefault,
			_Utils_Tuple2(index, $rluiten$sparsevector$SparseVector$empty),
			A2(
				$elm$core$Maybe$map,
				function (tokenSet) {
					return A3(
						$elm$core$List$foldr,
						$rluiten$elm_text_search$Index$Vector$updateDocVector(docRef),
						_Utils_Tuple2(index, $rluiten$sparsevector$SparseVector$empty),
						$elm$core$Set$toList(tokenSet));
				},
				A2($elm$core$Dict$get, docRef, irec.H)));
	});
var $rluiten$elm_text_search$Index$Vector$scoreAndCompare = F3(
	function (queryVector, ref, _v0) {
		var index = _v0.a;
		var docs = _v0.b;
		var _v1 = A2($rluiten$elm_text_search$Index$Vector$getDocVector, index, ref);
		var u1index = _v1.a;
		var docVector = _v1.b;
		return _Utils_Tuple2(
			u1index,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					ref,
					A2($rluiten$sparsevector$SparseVector$cosineSimilarity, queryVector, docVector)),
				docs));
	});
var $rluiten$elm_text_search$Index$searchTokens = F2(
	function (tokens, index) {
		var irec = index;
		var fieldBoosts = $elm$core$List$sum(
			A2($elm$core$List$map, $elm$core$Tuple$second, irec.aY));
		var _v0 = A3($rluiten$elm_text_search$Index$Vector$getQueryVector, fieldBoosts, tokens, index);
		var tokenDocSets = _v0.a;
		var queryVector = _v0.b;
		var u1index = _v0.c;
		var _v1 = A3(
			$elm$core$List$foldr,
			$rluiten$elm_text_search$Index$Vector$scoreAndCompare(queryVector),
			_Utils_Tuple2(u1index, _List_Nil),
			$elm$core$Set$toList(
				$rluiten$elm_text_search$Utils$intersectSets(tokenDocSets)));
		var u2index = _v1.a;
		var matchedDocs = _v1.b;
		return _Utils_Tuple2(
			u2index,
			$elm$core$List$reverse(
				A2($elm$core$List$sortBy, $elm$core$Tuple$second, matchedDocs)));
	});
var $rluiten$elm_text_search$Index$search = F2(
	function (query, index) {
		var _v0 = A2($rluiten$elm_text_search$Index$Utils$getTokens, index, query);
		var i1index = _v0.a;
		var i1irec = i1index;
		var tokens = _v0.b;
		var tokenInStore = function (token) {
			return !_Utils_eq(
				A2($rluiten$trie$Trie$getNode, token, i1irec.T),
				$elm$core$Maybe$Nothing);
		};
		return $elm$core$Dict$isEmpty(i1irec.H) ? $elm$core$Result$Err('Error there are no documents in index to search.') : ($elm$core$String$isEmpty(
			$elm$core$String$trim(query)) ? $elm$core$Result$Err('Error query is empty.') : ($elm$core$List$isEmpty(tokens) ? $elm$core$Result$Err('Error after tokenisation there are no terms to search for.') : (($elm$core$List$isEmpty(tokens) || (!A2($elm$core$List$any, tokenInStore, tokens))) ? $elm$core$Result$Ok(
			_Utils_Tuple2(i1index, _List_Nil)) : $elm$core$Result$Ok(
			A2($rluiten$elm_text_search$Index$searchTokens, tokens, i1index)))));
	});
var $rluiten$elm_text_search$ElmTextSearch$search = $rluiten$elm_text_search$Index$search;
var $author$project$Update$search4words = F2(
	function (index, searchString) {
		var _v0 = A2($rluiten$elm_text_search$ElmTextSearch$search, searchString, index);
		if (_v0.$ === 1) {
			var msg = _v0.a;
			return _Utils_Tuple2(_List_Nil, index);
		} else {
			var _v1 = _v0.a;
			var newIndex = _v1.a;
			var resultList = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$map, $elm$core$Tuple$first, resultList),
				newIndex);
		}
	});
var $author$project$Data$subjectsOffered = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			1,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			2,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			3,
			_List_fromArray(
				[32])),
			_Utils_Tuple2(
			4,
			_List_fromArray(
				[42])),
			_Utils_Tuple2(
			5,
			_List_fromArray(
				[23, 33, 34, 39, 121, 129, 131])),
			_Utils_Tuple2(
			6,
			_List_fromArray(
				[13, 23, 28, 34, 36, 42, 44, 46, 53, 64, 66, 75, 78, 79, 86, 113, 118, 121, 129, 137])),
			_Utils_Tuple2(
			7,
			_List_fromArray(
				[1, 2, 21, 23, 27, 28, 33, 34, 39, 42, 46, 53, 57, 61, 67, 75, 78, 79, 81, 86, 89, 91, 92, 98, 106, 121, 128, 129, 131, 135, 137])),
			_Utils_Tuple2(
			8,
			_List_fromArray(
				[12])),
			_Utils_Tuple2(
			9,
			_List_fromArray(
				[22, 23, 34, 62, 75, 79, 121])),
			_Utils_Tuple2(
			10,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			11,
			_List_fromArray(
				[13, 28, 39, 42])),
			_Utils_Tuple2(
			12,
			_List_fromArray(
				[1, 23, 24, 34, 45, 47, 49, 75, 81, 84, 85, 98, 100, 111, 118, 121, 129, 130, 132])),
			_Utils_Tuple2(
			13,
			_List_fromArray(
				[73, 93, 135])),
			_Utils_Tuple2(
			14,
			_List_fromArray(
				[1, 2, 23, 34, 36, 44, 46, 53, 64, 75, 78, 86, 89, 93, 121, 129, 131, 133, 137])),
			_Utils_Tuple2(
			15,
			_List_fromArray(
				[13, 23, 34, 36, 39, 42, 46, 64, 78, 86, 93, 103, 113, 121, 129])),
			_Utils_Tuple2(
			16,
			_List_fromArray(
				[1, 15, 23, 34, 45, 64, 75, 79, 86, 105, 112, 118, 121])),
			_Utils_Tuple2(
			17,
			_List_fromArray(
				[1, 2, 12, 13, 21, 23, 27, 28, 33, 34, 39, 42, 45, 46, 47, 50, 53, 61, 71, 75, 79, 81, 85, 86, 87, 89, 91, 92, 93, 103, 120, 121, 128, 129, 131, 133])),
			_Utils_Tuple2(
			18,
			_List_fromArray(
				[46, 133])),
			_Utils_Tuple2(
			19,
			_List_fromArray(
				[39])),
			_Utils_Tuple2(
			20,
			_List_fromArray(
				[23])),
			_Utils_Tuple2(
			21,
			_List_fromArray(
				[1, 2, 9, 10, 23, 28, 33, 34, 39, 50, 53, 57, 61, 73, 75, 79, 81, 86, 91, 100, 129, 131, 135])),
			_Utils_Tuple2(
			22,
			_List_fromArray(
				[46, 133])),
			_Utils_Tuple2(
			23,
			_List_fromArray(
				[88])),
			_Utils_Tuple2(
			24,
			_List_fromArray(
				[1, 3, 23, 28, 29, 33, 34, 35, 39, 42, 45, 46, 47, 50, 53, 64, 75, 84, 85, 86, 92, 116, 118, 121, 129, 131])),
			_Utils_Tuple2(
			25,
			_List_fromArray(
				[2, 27, 34, 39, 67, 89, 121, 129, 131, 135])),
			_Utils_Tuple2(
			26,
			_List_fromArray(
				[1, 2, 23, 27, 28, 34, 35, 46, 57, 61, 64, 75, 81, 86, 87, 91, 93, 118, 121, 129, 131, 133])),
			_Utils_Tuple2(
			27,
			_List_fromArray(
				[1, 13, 23, 34, 39, 41, 46, 61, 81, 96, 100, 121, 131, 135])),
			_Utils_Tuple2(
			28,
			_List_fromArray(
				[1, 10, 12, 19, 23, 25, 29, 34, 35, 38, 43, 45, 49, 50, 53, 57, 58, 64, 66, 69, 73, 75, 78, 84, 85, 86, 87, 88, 92, 93, 95, 98, 100, 111, 112, 113, 115, 116, 118, 129, 134])),
			_Utils_Tuple2(
			29,
			_List_fromArray(
				[1, 2, 23, 27, 29, 34, 45, 53, 61, 73, 75, 79, 84, 85, 87, 91, 98, 118, 121, 129])),
			_Utils_Tuple2(
			30,
			_List_fromArray(
				[65])),
			_Utils_Tuple2(
			31,
			_List_fromArray(
				[1, 2, 3, 7, 10, 12, 13, 21, 23, 26, 28, 29, 34, 35, 39, 45, 47, 48, 50, 53, 57, 61, 64, 66, 67, 75, 79, 81, 84, 85, 86, 96, 113, 116, 118, 121, 126, 128, 129, 131, 135])),
			_Utils_Tuple2(
			32,
			_List_fromArray(
				[129, 131])),
			_Utils_Tuple2(
			33,
			_List_fromArray(
				[1, 2, 12, 13, 23, 28, 33, 34, 39, 42, 45, 46, 53, 57, 61, 63, 64, 67, 71, 73, 75, 79, 81, 85, 86, 89, 91, 108, 111, 118, 121, 126, 129, 130])),
			_Utils_Tuple2(
			34,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			35,
			_List_fromArray(
				[37, 93])),
			_Utils_Tuple2(
			36,
			_List_fromArray(
				[2, 23, 27, 28, 34, 42, 46, 75, 79, 86, 89, 93, 106, 113, 121, 126, 128, 129, 131, 133])),
			_Utils_Tuple2(
			37,
			_List_fromArray(
				[1, 2, 18, 21, 23, 27, 28, 34, 39, 48, 50, 58, 71, 73, 75, 79, 81, 86, 89, 90, 91, 93, 121, 129, 130, 131, 135])),
			_Utils_Tuple2(
			38,
			_List_fromArray(
				[134])),
			_Utils_Tuple2(
			39,
			_List_fromArray(
				[13, 23, 28, 33, 36, 39, 42, 81, 86, 93])),
			_Utils_Tuple2(
			40,
			_List_fromArray(
				[39, 81])),
			_Utils_Tuple2(
			41,
			_List_fromArray(
				[39, 93])),
			_Utils_Tuple2(
			42,
			_List_fromArray(
				[1, 2, 10, 21, 23, 27, 29, 34, 35, 39, 47, 48, 53, 61, 76, 79, 81, 85, 86, 87, 89, 98, 100, 116, 128, 129, 130, 135])),
			_Utils_Tuple2(
			43,
			_List_fromArray(
				[13, 39])),
			_Utils_Tuple2(
			44,
			_List_fromArray(
				[9, 13, 34, 39, 42, 45, 46, 50, 64, 65, 79, 81, 86, 93, 118, 121, 126, 129])),
			_Utils_Tuple2(
			45,
			_List_fromArray(
				[42, 93])),
			_Utils_Tuple2(
			46,
			_List_fromArray(
				[5, 8, 85, 109, 124])),
			_Utils_Tuple2(
			47,
			_List_fromArray(
				[8, 101, 131, 135])),
			_Utils_Tuple2(
			48,
			_List_fromArray(
				[39])),
			_Utils_Tuple2(
			49,
			_List_fromArray(
				[1, 18, 21, 24, 25, 29, 34, 39, 45, 47, 53, 79, 81, 84, 85, 105, 117, 121, 132])),
			_Utils_Tuple2(
			50,
			_List_fromArray(
				[88])),
			_Utils_Tuple2(
			51,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			52,
			_List_fromArray(
				[3, 18, 23, 24, 25, 29, 34, 43, 47, 48, 61, 83, 84, 85, 88, 92, 104, 115, 132])),
			_Utils_Tuple2(
			53,
			_List_fromArray(
				[23, 39])),
			_Utils_Tuple2(
			54,
			_List_fromArray(
				[23, 75])),
			_Utils_Tuple2(
			55,
			_List_fromArray(
				[2, 7, 17, 19, 25, 27, 30, 34, 38, 40, 45, 47, 50, 58, 61, 64, 66, 69, 75, 79, 84, 86, 88, 91, 92, 93, 105, 111, 112, 115, 118, 121, 126, 129, 134])),
			_Utils_Tuple2(
			56,
			_List_fromArray(
				[1, 2, 3, 12, 13, 18, 23, 25, 27, 28, 29, 39, 45, 65, 71, 75, 84, 85, 86, 91, 110, 111, 118, 121, 128, 129, 131])),
			_Utils_Tuple2(
			57,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			58,
			_List_fromArray(
				[42])),
			_Utils_Tuple2(
			59,
			_List_fromArray(
				[13, 28, 39])),
			_Utils_Tuple2(
			60,
			_List_fromArray(
				[1, 2, 12, 13, 21, 23, 28, 29, 34, 39, 45, 46, 61, 64, 66, 71, 73, 75, 78, 79, 81, 86, 93, 96, 108, 118, 121, 128, 129, 131, 133, 135])),
			_Utils_Tuple2(
			61,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			62,
			_List_fromArray(
				[10, 23, 46, 62, 81, 86, 129, 131, 133])),
			_Utils_Tuple2(
			63,
			_List_fromArray(
				[23, 42, 46, 121, 127, 129, 131, 133])),
			_Utils_Tuple2(
			64,
			_List_fromArray(
				[1, 2, 10, 12, 13, 18, 21, 23, 28, 29, 34, 36, 39, 42, 46, 47, 50, 57, 61, 64, 67, 73, 75, 81, 84, 85, 86, 89, 111, 113, 121, 128, 129, 131, 133, 135, 137])),
			_Utils_Tuple2(
			65,
			_List_fromArray(
				[1, 12, 23, 34, 39, 46, 71, 75, 79, 81, 86, 96, 100, 118, 121, 126, 129, 131])),
			_Utils_Tuple2(
			66,
			_List_fromArray(
				[134])),
			_Utils_Tuple2(
			67,
			_List_fromArray(
				[1, 2, 12, 21, 23, 24, 27, 28, 29, 34, 35, 39, 42, 46, 47, 53, 75, 81, 85, 87, 89, 91, 121, 129])),
			_Utils_Tuple2(
			68,
			_List_fromArray(
				[37])),
			_Utils_Tuple2(
			69,
			_List_fromArray(
				[131])),
			_Utils_Tuple2(
			70,
			_List_fromArray(
				[1, 3, 10, 13, 21, 23, 24, 25, 29, 34, 39, 45, 47, 48, 50, 64, 66, 79, 83, 84, 85, 86, 113, 115, 118, 120, 121, 129, 131])),
			_Utils_Tuple2(
			71,
			_List_fromArray(
				[1, 2, 12, 13, 18, 23, 25, 28, 33, 34, 35, 36, 39, 45, 46, 47, 50, 52, 53, 64, 67, 71, 75, 77, 79, 81, 84, 85, 86, 96, 100, 112, 116, 118, 121, 128, 129, 130, 131, 133, 135])),
			_Utils_Tuple2(
			72,
			_List_fromArray(
				[7, 110, 111])),
			_Utils_Tuple2(
			73,
			_List_fromArray(
				[28])),
			_Utils_Tuple2(
			74,
			_List_fromArray(
				[1, 2, 19, 23, 27, 28, 34, 37, 39, 41, 45, 46, 53, 61, 71, 75, 79, 81, 86, 89, 91, 93, 109, 118, 121, 126, 128, 129, 131, 135])),
			_Utils_Tuple2(
			75,
			_List_fromArray(
				[134])),
			_Utils_Tuple2(
			76,
			_List_fromArray(
				[42])),
			_Utils_Tuple2(
			77,
			_List_fromArray(
				[35, 46, 121, 128, 133])),
			_Utils_Tuple2(
			78,
			_List_fromArray(
				[46])),
			_Utils_Tuple2(
			79,
			_List_fromArray(
				[13, 28, 39])),
			_Utils_Tuple2(
			80,
			_List_fromArray(
				[1, 8, 12, 13, 18, 21, 23, 25, 28, 29, 34, 39, 42, 45, 46, 53, 62, 64, 67, 69, 73, 75, 77, 78, 79, 81, 84, 86, 100, 112, 113, 115, 118, 121, 129, 131, 133])),
			_Utils_Tuple2(
			81,
			_List_fromArray(
				[32])),
			_Utils_Tuple2(
			82,
			_List_fromArray(
				[1, 2, 7, 9, 12, 18, 21, 34, 45, 46, 50, 61, 64, 66, 75, 79, 81, 85, 86, 92, 112, 117, 118, 121, 129, 131, 133, 135])),
			_Utils_Tuple2(
			83,
			_List_fromArray(
				[23, 33])),
			_Utils_Tuple2(
			84,
			_List_fromArray(
				[28, 39])),
			_Utils_Tuple2(
			85,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			86,
			_List_fromArray(
				[23, 34, 102])),
			_Utils_Tuple2(
			87,
			_List_fromArray(
				[23, 42, 86, 121, 123, 129, 135])),
			_Utils_Tuple2(
			88,
			_List_fromArray(
				[1, 3, 18, 23, 25, 34, 38, 42, 45, 47, 50, 53, 58, 59, 64, 66, 75, 77, 79, 81, 84, 85, 86, 88, 92, 105, 115, 118, 121])),
			_Utils_Tuple2(
			89,
			_List_fromArray(
				[1, 2, 3, 7, 18, 24, 27, 29, 34, 38, 45, 47, 50, 53, 58, 64, 66, 69, 71, 75, 79, 84, 85, 88, 91, 93, 111, 112, 113, 115, 118, 121, 128, 129, 130, 134])),
			_Utils_Tuple2(
			90,
			_List_fromArray(
				[12, 28, 39, 81, 86])),
			_Utils_Tuple2(
			91,
			_List_fromArray(
				[23, 53, 69, 79, 81, 127])),
			_Utils_Tuple2(
			92,
			_List_fromArray(
				[118])),
			_Utils_Tuple2(
			93,
			_List_fromArray(
				[1, 2, 12, 13, 23, 34, 39, 75, 79, 85, 91, 111, 128, 129, 135])),
			_Utils_Tuple2(
			94,
			_List_fromArray(
				[20, 23, 28, 36, 37, 42, 46, 73, 75, 78, 121, 129, 131, 133])),
			_Utils_Tuple2(
			95,
			_List_fromArray(
				[42])),
			_Utils_Tuple2(
			96,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			97,
			_List_fromArray(
				[42, 93])),
			_Utils_Tuple2(
			98,
			_List_fromArray(
				[1, 23, 28, 30, 34, 42, 43, 45, 50, 53, 64, 66, 75, 79, 81, 84, 86, 92, 93, 112, 113, 118, 121, 129])),
			_Utils_Tuple2(
			99,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			100,
			_List_fromArray(
				[42, 93])),
			_Utils_Tuple2(
			101,
			_List_fromArray(
				[28, 33, 86, 93, 108])),
			_Utils_Tuple2(
			102,
			_List_fromArray(
				[1, 2, 3, 12, 18, 21, 23, 27, 28, 34, 35, 36, 39, 45, 46, 47, 49, 52, 53, 54, 61, 64, 66, 67, 71, 73, 75, 78, 79, 81, 84, 85, 86, 87, 89, 91, 96, 100, 105, 113, 116, 121, 128, 129, 130, 131, 133, 135])),
			_Utils_Tuple2(
			103,
			_List_fromArray(
				[23, 28, 33, 39, 73, 79, 81, 85, 86, 93, 108, 121, 128, 129, 131, 135])),
			_Utils_Tuple2(
			104,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			105,
			_List_fromArray(
				[127, 133])),
			_Utils_Tuple2(
			106,
			_List_fromArray(
				[23, 42, 61, 75, 121, 131, 133])),
			_Utils_Tuple2(
			107,
			_List_fromArray(
				[134])),
			_Utils_Tuple2(
			108,
			_List_fromArray(
				[61, 87, 88, 116])),
			_Utils_Tuple2(
			109,
			_List_fromArray(
				[23])),
			_Utils_Tuple2(
			110,
			_List_fromArray(
				[2, 23, 28, 33, 34, 39, 42, 46, 57, 73, 75, 85, 86, 121, 128, 129, 131])),
			_Utils_Tuple2(
			111,
			_List_fromArray(
				[46, 131, 133])),
			_Utils_Tuple2(
			112,
			_List_fromArray(
				[1, 2, 3, 6, 7, 18, 23, 24, 29, 30, 34, 45, 47, 49, 53, 59, 64, 66, 75, 78, 83, 84, 85, 86, 88, 89, 92, 113, 115, 118, 121, 129, 130, 131, 137])),
			_Utils_Tuple2(
			113,
			_List_fromArray(
				[2, 23, 27, 29, 34, 39, 46, 47, 57, 61, 75, 76, 85, 87, 89, 100, 116, 121, 128, 129, 131])),
			_Utils_Tuple2(
			114,
			_List_fromArray(
				[32])),
			_Utils_Tuple2(
			115,
			_List_fromArray(
				[37])),
			_Utils_Tuple2(
			116,
			_List_fromArray(
				[42, 93])),
			_Utils_Tuple2(
			117,
			_List_fromArray(
				[53])),
			_Utils_Tuple2(
			118,
			_List_fromArray(
				[1, 9, 45, 53, 64, 66, 75, 79, 84, 112, 118, 129, 132])),
			_Utils_Tuple2(
			119,
			_List_fromArray(
				[39])),
			_Utils_Tuple2(
			120,
			_List_fromArray(
				[1, 2, 18, 29, 30, 31, 35, 36, 39, 45, 46, 48, 50, 61, 64, 68, 75, 78, 84, 89, 100, 103, 105, 107, 108, 112, 113, 114, 115, 118, 127, 128, 132])),
			_Utils_Tuple2(
			121,
			_List_fromArray(
				[93])),
			_Utils_Tuple2(
			122,
			_List_fromArray(
				[5, 8, 124])),
			_Utils_Tuple2(
			123,
			_List_fromArray(
				[8, 136])),
			_Utils_Tuple2(
			124,
			_List_fromArray(
				[4, 9, 15, 40, 45, 64, 75, 79, 118])),
			_Utils_Tuple2(
			125,
			_List_fromArray(
				[1, 3, 12, 18, 23, 24, 25, 29, 31, 34, 45, 47, 53, 58, 69, 79, 81, 84, 85, 92, 100, 111, 115, 118, 121, 126, 129, 131])),
			_Utils_Tuple2(
			126,
			_List_fromArray(
				[1, 18, 23, 24, 25, 29, 30, 34, 36, 38, 42, 43, 45, 46, 47, 49, 50, 53, 61, 64, 66, 75, 79, 81, 83, 84, 85, 88, 92, 93, 95, 105, 111, 112, 113, 115, 116, 118, 121, 126, 128, 129, 131, 134])),
			_Utils_Tuple2(
			127,
			_List_fromArray(
				[2, 28, 33, 85, 103, 121, 129, 131])),
			_Utils_Tuple2(
			128,
			_List_fromArray(
				[1, 2, 17, 23, 29, 34, 35, 57, 61, 75, 98, 100, 111, 116, 121, 128, 129])),
			_Utils_Tuple2(
			129,
			_List_fromArray(
				[23, 45, 50, 75, 88, 118, 121])),
			_Utils_Tuple2(
			130,
			_List_fromArray(
				[37, 42, 64, 86, 93, 131, 133])),
			_Utils_Tuple2(
			131,
			_List_fromArray(
				[2, 35, 46, 87, 128, 131, 133])),
			_Utils_Tuple2(
			132,
			_List_fromArray(
				[1, 2, 6, 18, 23, 34, 36, 42, 43, 45, 46, 52, 53, 61, 64, 66, 75, 78, 79, 81, 84, 86, 88, 90, 92, 105, 111, 112, 113, 118, 121, 131])),
			_Utils_Tuple2(
			133,
			_List_fromArray(
				[1, 2, 6, 28, 34, 42, 45, 53, 61, 64, 75, 78, 79, 81, 84, 89, 105, 107, 112, 118, 121, 129, 131])),
			_Utils_Tuple2(
			134,
			_List_fromArray(
				[1, 2, 7, 12, 18, 23, 25, 26, 28, 32, 34, 35, 39, 42, 46, 47, 49, 53, 64, 71, 73, 75, 78, 79, 81, 85, 86, 89, 91, 92, 93, 95, 103, 107, 108, 110, 111, 116, 118, 121, 128, 129, 130, 131, 135])),
			_Utils_Tuple2(
			135,
			_List_fromArray(
				[1, 2, 6, 18, 23, 24, 25, 33, 34, 36, 43, 44, 45, 46, 50, 53, 64, 75, 79, 81, 85, 93, 112, 113, 115, 118, 121, 128, 129, 131, 133, 137])),
			_Utils_Tuple2(
			136,
			_List_fromArray(
				[1, 9, 12, 16, 18, 23, 25, 28, 30, 34, 39, 42, 45, 47, 50, 53, 57, 58, 64, 75, 77, 79, 81, 84, 93, 105, 112, 115, 118, 121, 126, 128, 129, 131])),
			_Utils_Tuple2(
			137,
			_List_fromArray(
				[1, 13, 18, 23, 34, 45, 49, 50, 52, 53, 61, 64, 66, 75, 77, 81, 84, 85, 86, 88, 112, 114, 115, 118, 121, 129])),
			_Utils_Tuple2(
			138,
			_List_fromArray(
				[75])),
			_Utils_Tuple2(
			139,
			_List_fromArray(
				[1, 2, 3, 7, 13, 15, 18, 19, 23, 24, 25, 27, 29, 30, 34, 38, 39, 42, 43, 45, 46, 47, 49, 50, 53, 56, 58, 60, 64, 65, 66, 68, 69, 75, 77, 78, 79, 81, 84, 85, 86, 87, 88, 91, 92, 93, 96, 112, 113, 115, 118, 120, 121, 126, 127, 128, 129, 131, 134])),
			_Utils_Tuple2(
			140,
			_List_fromArray(
				[3, 7, 11, 25, 34, 43, 45, 50, 53, 64, 66, 75, 79, 81, 84, 85, 86, 88, 92, 105, 113, 115, 118, 121, 129])),
			_Utils_Tuple2(
			141,
			_List_fromArray(
				[1, 3, 7, 11, 12, 18, 23, 25, 29, 31, 34, 35, 38, 43, 45, 47, 50, 53, 58, 66, 69, 75, 78, 79, 81, 84, 85, 86, 87, 88, 92, 93, 95, 100, 112, 113, 115, 116, 117, 120, 121, 129, 130, 136])),
			_Utils_Tuple2(
			142,
			_List_fromArray(
				[1, 2, 3, 6, 9, 15, 18, 19, 23, 24, 25, 27, 29, 30, 34, 38, 39, 45, 47, 49, 51, 52, 53, 58, 60, 61, 64, 66, 69, 75, 77, 78, 79, 81, 82, 84, 85, 88, 89, 90, 91, 92, 93, 98, 100, 105, 111, 112, 113, 115, 118, 119, 121, 126, 129, 132, 134])),
			_Utils_Tuple2(
			143,
			_List_fromArray(
				[1, 7, 12, 13, 18, 21, 23, 25, 28, 30, 34, 39, 42, 45, 50, 53, 56, 58, 64, 66, 69, 75, 79, 81, 84, 111, 112, 113, 118, 121, 133, 137])),
			_Utils_Tuple2(
			144,
			_List_fromArray(
				[1, 3, 11, 15, 17, 18, 24, 25, 29, 34, 38, 45, 46, 47, 49, 50, 58, 59, 60, 61, 64, 66, 69, 73, 74, 75, 78, 79, 83, 84, 85, 88, 92, 105, 112, 113, 115, 117, 118, 120, 121, 126, 129])),
			_Utils_Tuple2(
			145,
			_List_fromArray(
				[1, 2, 3, 24, 25, 27, 29, 34, 42, 45, 47, 53, 61, 75, 78, 79, 84, 85, 91, 92, 93, 96, 115, 118, 121, 129, 135, 136])),
			_Utils_Tuple2(
			146,
			_List_fromArray(
				[1, 18, 23, 25, 34, 40, 42, 45, 47, 48, 50, 53, 58, 60, 61, 64, 75, 77, 78, 79, 84, 85, 88, 92, 97, 105, 112, 115, 118, 121, 127, 129, 132])),
			_Utils_Tuple2(
			147,
			_List_fromArray(
				[1, 2, 23, 29, 34, 39, 42, 57, 62, 75, 86, 89, 91, 93, 95, 102, 121, 129, 135])),
			_Utils_Tuple2(
			148,
			_List_fromArray(
				[1, 12, 13, 18, 21, 23, 28, 34, 39, 47, 49, 64, 69, 73, 75, 78, 79, 81, 86, 93, 103, 118, 121, 123, 129, 135])),
			_Utils_Tuple2(
			149,
			_List_fromArray(
				[37, 93])),
			_Utils_Tuple2(
			150,
			_List_fromArray(
				[70, 100, 101])),
			_Utils_Tuple2(
			151,
			_List_fromArray(
				[23, 42, 46, 55, 81, 128, 131, 135])),
			_Utils_Tuple2(
			152,
			_List_fromArray(
				[7, 9, 12, 13, 18, 19, 23, 24, 25, 29, 30, 34, 43, 45, 47, 53, 57, 58, 60, 61, 64, 65, 66, 69, 75, 77, 78, 79, 84, 85, 88, 92, 105, 110, 111, 112, 115, 117, 118, 121, 125, 127, 129, 132, 133])),
			_Utils_Tuple2(
			153,
			_List_fromArray(
				[21, 29, 79])),
			_Utils_Tuple2(
			154,
			_List_fromArray(
				[32])),
			_Utils_Tuple2(
			155,
			_List_fromArray(
				[12, 13, 23, 28, 39, 42, 81, 103])),
			_Utils_Tuple2(
			156,
			_List_fromArray(
				[1, 7, 9, 19, 24, 43, 45, 50, 53, 59, 64, 66, 75, 79, 85, 88, 100, 110, 113, 118, 121, 129, 133, 137])),
			_Utils_Tuple2(
			157,
			_List_fromArray(
				[1, 2, 23, 34, 46, 53, 75, 89, 91, 121, 126, 129, 131, 133])),
			_Utils_Tuple2(
			158,
			_List_fromArray(
				[1, 2, 3, 10, 12, 13, 23, 28, 29, 34, 39, 46, 49, 53, 61, 73, 75, 78, 81, 85, 86, 91, 100, 111, 112, 113, 116, 121, 126, 129, 131, 133, 135])),
			_Utils_Tuple2(
			159,
			_List_fromArray(
				[1, 3, 7, 9, 18, 19, 25, 28, 29, 34, 38, 42, 43, 45, 46, 47, 50, 53, 58, 60, 69, 72, 75, 79, 84, 85, 88, 92, 93, 101, 113, 115, 118, 121, 126, 129, 134, 136, 137])),
			_Utils_Tuple2(
			160,
			_List_fromArray(
				[88])),
			_Utils_Tuple2(
			161,
			_List_fromArray(
				[1, 2, 15, 18, 21, 27, 28, 34, 35, 39, 42, 57, 73, 75, 79, 85, 86, 88, 89, 111, 121, 126, 128, 129, 131, 135])),
			_Utils_Tuple2(
			162,
			_List_fromArray(
				[1, 2, 8, 18, 23, 39, 46, 61, 64, 69, 75, 78, 81, 89, 113, 118, 121, 126, 129, 131, 133, 135])),
			_Utils_Tuple2(
			163,
			_List_fromArray(
				[1, 2, 28, 34, 35, 39, 46, 57, 64, 73, 75, 79, 81, 85, 86, 89, 95, 121, 128, 129, 131, 133, 135])),
			_Utils_Tuple2(
			164,
			_List_fromArray(
				[1, 2, 7, 12, 13, 23, 34, 38, 39, 45, 46, 50, 64, 75, 84, 85, 88, 100, 118, 121, 133])),
			_Utils_Tuple2(
			165,
			_List_fromArray(
				[1, 9, 10, 11, 18, 23, 25, 30, 34, 43, 45, 46, 48, 53, 61, 64, 66, 75, 77, 78, 79, 81, 84, 93, 100, 101, 105, 112, 113, 115, 118, 121, 129, 131, 133, 134])),
			_Utils_Tuple2(
			166,
			_List_fromArray(
				[1, 2, 12, 29, 34, 37, 39, 46, 51, 61, 75, 79, 86, 116, 118, 121, 128, 129, 131, 135])),
			_Utils_Tuple2(
			167,
			_List_fromArray(
				[1, 7, 9, 12, 13, 14, 15, 18, 23, 24, 25, 29, 30, 34, 39, 43, 44, 45, 47, 53, 58, 64, 65, 66, 69, 75, 77, 78, 79, 84, 85, 88, 90, 92, 105, 112, 113, 115, 118, 121, 129, 132, 133, 134, 135, 136, 137])),
			_Utils_Tuple2(
			168,
			_List_fromArray(
				[1, 18, 29, 30, 31, 34, 42, 44, 45, 50, 61, 64, 66, 75, 79, 81, 84, 85, 87, 88, 105, 112, 113, 115, 118, 121, 129, 131, 134, 137])),
			_Utils_Tuple2(
			169,
			_List_fromArray(
				[1, 3, 7, 16, 19, 23, 25, 28, 29, 34, 38, 39, 42, 45, 47, 49, 53, 58, 59, 64, 65, 66, 69, 75, 78, 79, 84, 85, 88, 90, 92, 93, 112, 115, 118, 121, 126, 129, 130, 132, 133, 136, 137])),
			_Utils_Tuple2(
			170,
			_List_fromArray(
				[2, 28, 33, 36, 39, 42, 46, 73, 79, 81, 86, 95, 121, 129, 131, 133, 135])),
			_Utils_Tuple2(
			171,
			_List_fromArray(
				[1, 2, 12, 21, 23, 27, 34, 39, 45, 46, 53, 61, 71, 75, 79, 81, 84, 86, 89, 91, 92, 105, 107, 118, 121, 129, 133, 135])),
			_Utils_Tuple2(
			172,
			_List_fromArray(
				[1, 2, 3, 12, 23, 27, 28, 31, 34, 39, 45, 46, 53, 61, 64, 75, 81, 84, 85, 87, 89, 91, 93, 110, 111, 121, 128, 131, 133, 135])),
			_Utils_Tuple2(
			173,
			_List_fromArray(
				[1, 2, 18, 19, 25, 34, 45, 50, 53, 64, 66, 75, 79, 81, 84, 88, 111, 116, 118, 121, 129])),
			_Utils_Tuple2(
			174,
			_List_fromArray(
				[1, 2, 8, 13, 23, 25, 33, 34, 39, 42, 50, 53, 57, 61, 64, 73, 75, 79, 81, 85, 86, 118, 121, 129, 131, 135])),
			_Utils_Tuple2(
			175,
			_List_fromArray(
				[1, 5, 11, 12, 13, 18, 24, 25, 29, 30, 31, 34, 38, 45, 47, 50, 58, 60, 61, 64, 66, 69, 75, 78, 79, 80, 81, 84, 85, 86, 88, 93, 110, 111, 112, 113, 115, 117, 118, 121, 129, 131, 137])),
			_Utils_Tuple2(
			176,
			_List_fromArray(
				[1, 2, 23, 35, 39, 42, 46, 75, 79, 81, 89, 121, 127, 129, 131, 133, 135])),
			_Utils_Tuple2(
			177,
			_List_fromArray(
				[1, 2, 12, 13, 18, 21, 23, 25, 27, 28, 29, 34, 35, 39, 47, 50, 64, 66, 71, 73, 75, 85, 89, 91, 113, 116, 118, 121, 128, 129, 131, 133])),
			_Utils_Tuple2(
			178,
			_List_fromArray(
				[1, 2, 6, 7, 12, 14, 18, 19, 21, 23, 24, 25, 28, 29, 30, 34, 45, 47, 50, 53, 58, 64, 66, 69, 70, 75, 79, 84, 85, 86, 88, 91, 92, 96, 105, 111, 112, 113, 115, 116, 118, 120, 121, 129, 134, 136])),
			_Utils_Tuple2(
			179,
			_List_fromArray(
				[1, 2, 7, 12, 21, 23, 27, 28, 29, 34, 35, 38, 39, 42, 43, 44, 45, 46, 47, 50, 52, 64, 75, 79, 81, 84, 85, 88, 89, 91, 96, 98, 113, 116, 118, 121, 129, 133, 135])),
			_Utils_Tuple2(
			180,
			_List_fromArray(
				[1, 2, 12, 16, 21, 23, 28, 29, 34, 36, 39, 43, 45, 46, 47, 49, 53, 61, 64, 67, 71, 73, 75, 79, 81, 84, 85, 86, 87, 92, 95, 105, 111, 113, 115, 118, 121, 128, 129, 131])),
			_Utils_Tuple2(
			181,
			_List_fromArray(
				[1, 2, 3, 12, 21, 23, 27, 28, 32, 34, 35, 36, 39, 42, 50, 53, 61, 73, 75, 79, 85, 86, 87, 89, 91, 93, 116, 121, 128, 129, 131, 137])),
			_Utils_Tuple2(
			182,
			_List_fromArray(
				[1, 3, 18, 21, 23, 28, 32, 34, 35, 39, 42, 57, 64, 71, 75, 81, 85, 86, 89, 93, 95, 121, 129, 131, 133])),
			_Utils_Tuple2(
			183,
			_List_fromArray(
				[1, 2, 3, 11, 13, 18, 23, 25, 27, 29, 34, 39, 43, 45, 47, 53, 58, 64, 66, 69, 75, 78, 79, 84, 85, 86, 88, 92, 100, 112, 113, 115, 118, 121, 129])),
			_Utils_Tuple2(
			184,
			_List_fromArray(
				[4, 9, 23, 25, 34, 45, 50, 64, 65, 66, 69, 84, 86, 88, 112, 113, 115, 118, 121])),
			_Utils_Tuple2(
			185,
			_List_fromArray(
				[86, 131, 133])),
			_Utils_Tuple2(
			186,
			_List_fromArray(
				[1, 2, 23, 34, 64, 75, 78, 81, 86, 118, 121, 129, 131, 133])),
			_Utils_Tuple2(
			187,
			_List_fromArray(
				[1, 12, 17, 23, 24, 25, 29, 34, 45, 46, 47, 50, 53, 58, 64, 67, 69, 73, 75, 81, 84, 85, 90, 94, 101, 105, 111, 115, 118, 120, 121, 126, 128, 132, 133])),
			_Utils_Tuple2(
			188,
			_List_fromArray(
				[2, 23, 87, 89, 121, 129])),
			_Utils_Tuple2(
			189,
			_List_fromArray(
				[1, 2, 18, 23, 28, 34, 39, 50, 62, 75, 86, 111, 121, 129, 131, 133, 135])),
			_Utils_Tuple2(
			190,
			_List_fromArray(
				[1, 6, 9, 14, 18, 19, 23, 25, 34, 40, 42, 45, 47, 50, 53, 64, 65, 66, 75, 79, 81, 84, 85, 86, 92, 105, 112, 113, 115, 118, 121, 122, 128, 129, 137])),
			_Utils_Tuple2(
			191,
			_List_fromArray(
				[1, 12, 13, 21, 23, 26, 28, 29, 34, 35, 39, 45, 48, 50, 53, 61, 62, 64, 67, 70, 71, 75, 76, 79, 81, 85, 86, 87, 96, 99, 106, 116, 118, 121, 123, 126, 128, 129, 130, 131, 135])),
			_Utils_Tuple2(
			192,
			_List_fromArray(
				[28, 39, 46, 79, 85, 126, 131, 133, 135])),
			_Utils_Tuple2(
			193,
			_List_fromArray(
				[11, 36, 42, 46, 64, 75, 78, 79, 81, 86, 121, 128, 129, 131, 133])),
			_Utils_Tuple2(
			194,
			_List_fromArray(
				[1, 2, 18, 21, 23, 26, 34, 35, 39, 46, 48, 50, 62, 64, 75, 79, 89, 91, 92, 93, 111, 121, 126, 131, 133])),
			_Utils_Tuple2(
			195,
			_List_fromArray(
				[2, 23, 35, 46, 61, 66, 75, 78, 86, 121, 129, 131, 133])),
			_Utils_Tuple2(
			196,
			_List_fromArray(
				[2, 11, 18, 25, 28, 34, 42, 45, 46, 47, 50, 59, 64, 65, 75, 77, 79, 84, 90, 92, 93, 112, 113, 115, 118, 121, 126, 129])),
			_Utils_Tuple2(
			197,
			_List_fromArray(
				[12, 13, 28, 39, 42, 73, 79, 81, 86, 123])),
			_Utils_Tuple2(
			198,
			_List_fromArray(
				[46, 62, 93, 95, 131])),
			_Utils_Tuple2(
			199,
			_List_fromArray(
				[1, 2, 3, 12, 13, 21, 23, 27, 28, 34, 35, 39, 42, 44, 45, 46, 47, 50, 53, 61, 64, 66, 71, 73, 75, 79, 81, 85, 86, 87, 89, 91, 100, 108, 113, 116, 117, 118, 121, 128, 129, 130, 133])),
			_Utils_Tuple2(
			200,
			_List_fromArray(
				[1, 2, 23, 26, 29, 34, 61, 71, 75, 81, 86, 89, 91, 93, 100, 118, 121, 128, 129, 131, 133, 135])),
			_Utils_Tuple2(
			201,
			_List_fromArray(
				[42])),
			_Utils_Tuple2(
			202,
			_List_fromArray(
				[137])),
			_Utils_Tuple2(
			203,
			_List_fromArray(
				[23, 46, 129])),
			_Utils_Tuple2(
			204,
			_List_fromArray(
				[8])),
			_Utils_Tuple2(
			205,
			_List_fromArray(
				[35, 39, 42, 46, 49, 64, 78, 79, 86, 93, 121, 129, 131, 133, 134]))
		]));
var $author$project$Data$uniCodes = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(1, 'ACM Guildford Limited'),
			_Utils_Tuple2(2, 'ACM London Limited'),
			_Utils_Tuple2(3, 'AECC University College'),
			_Utils_Tuple2(4, 'ALRA'),
			_Utils_Tuple2(5, 'Abertay University'),
			_Utils_Tuple2(6, 'Aberystwyth University'),
			_Utils_Tuple2(7, 'Anglia Ruskin University Higher Education Corporation'),
			_Utils_Tuple2(8, 'Architectural Association (Incorporated)'),
			_Utils_Tuple2(9, 'Arden University Limited'),
			_Utils_Tuple2(10, 'Arts Educational Schools(The)'),
			_Utils_Tuple2(11, 'Arts University Bournemouth, the'),
			_Utils_Tuple2(12, 'Aston University'),
			_Utils_Tuple2(13, 'BIMM Limited'),
			_Utils_Tuple2(14, 'Bangor University'),
			_Utils_Tuple2(15, 'Bath Spa University'),
			_Utils_Tuple2(16, 'Birkbeck College'),
			_Utils_Tuple2(17, 'Birmingham City University'),
			_Utils_Tuple2(18, 'Bishop Grosseteste University'),
			_Utils_Tuple2(19, 'Blackpool and the Fylde College'),
			_Utils_Tuple2(20, 'Bloomsbury Institute Limited'),
			_Utils_Tuple2(21, 'Bournemouth University'),
			_Utils_Tuple2(22, 'Bradford College'),
			_Utils_Tuple2(23, 'Brighton and Sussex Medical School'),
			_Utils_Tuple2(24, 'Brunel University London'),
			_Utils_Tuple2(25, 'Buckinghamshire New University'),
			_Utils_Tuple2(26, 'Canterbury Christ Church University'),
			_Utils_Tuple2(27, 'Cardiff Metropolitan University'),
			_Utils_Tuple2(28, 'Cardiff University'),
			_Utils_Tuple2(29, 'City, University of London'),
			_Utils_Tuple2(30, 'Courtauld Institute of Art'),
			_Utils_Tuple2(31, 'Coventry University'),
			_Utils_Tuple2(32, 'DN Colleges Group'),
			_Utils_Tuple2(33, 'De Montfort University'),
			_Utils_Tuple2(34, 'Deep Blue Sound Limited'),
			_Utils_Tuple2(35, 'Doreen Bird College of Performing Arts Ltd.'),
			_Utils_Tuple2(36, 'Edge Hill University'),
			_Utils_Tuple2(37, 'Edinburgh Napier University'),
			_Utils_Tuple2(38, 'Elim Foursquare Gospel Alliance'),
			_Utils_Tuple2(39, 'Falmouth University'),
			_Utils_Tuple2(40, 'Fashion Retail Academy'),
			_Utils_Tuple2(41, 'Futureworks Training Limited'),
			_Utils_Tuple2(42, 'Glasgow Caledonian University'),
			_Utils_Tuple2(43, 'Glasgow School of Art.'),
			_Utils_Tuple2(44, 'Goldsmiths\' College'),
			_Utils_Tuple2(45, 'Guildhall School of Music & Drama'),
			_Utils_Tuple2(46, 'Harper Adams University'),
			_Utils_Tuple2(47, 'Hartpury University'),
			_Utils_Tuple2(48, 'Hereford College of Arts'),
			_Utils_Tuple2(49, 'Heriot-Watt University'),
			_Utils_Tuple2(50, 'Hull and York Medical School'),
			_Utils_Tuple2(51, 'ICMP Management Limited'),
			_Utils_Tuple2(52, 'Imperial College of Science, Technology and Medicine'),
			_Utils_Tuple2(53, 'Istituto Marangoni Limited'),
			_Utils_Tuple2(54, 'Kaplan Open Learning (Essex) Limited'),
			_Utils_Tuple2(55, 'King\'s College London'),
			_Utils_Tuple2(56, 'Kingston University'),
			_Utils_Tuple2(57, 'LCCM AU UK Limited'),
			_Utils_Tuple2(58, 'LTE Group'),
			_Utils_Tuple2(59, 'Leeds Arts University'),
			_Utils_Tuple2(60, 'Leeds Beckett University'),
			_Utils_Tuple2(61, 'Leeds College of Music'),
			_Utils_Tuple2(62, 'Leeds Trinity University'),
			_Utils_Tuple2(63, 'Liverpool Hope University'),
			_Utils_Tuple2(64, 'Liverpool John Moores University'),
			_Utils_Tuple2(65, 'London Metropolitan University'),
			_Utils_Tuple2(66, 'London School of Theology'),
			_Utils_Tuple2(67, 'London South Bank University'),
			_Utils_Tuple2(68, 'London Studio Centre Limited'),
			_Utils_Tuple2(69, 'Loughborough College'),
			_Utils_Tuple2(70, 'Loughborough University'),
			_Utils_Tuple2(71, 'Manchester Metropolitan University'),
			_Utils_Tuple2(72, 'Medway School of Pharmacy'),
			_Utils_Tuple2(73, 'Met Film School Limited'),
			_Utils_Tuple2(74, 'Middlesex University'),
			_Utils_Tuple2(75, 'Moorlands College'),
			_Utils_Tuple2(76, 'Mountview Academy of Theatre Arts Limited'),
			_Utils_Tuple2(77, 'Newman University'),
			_Utils_Tuple2(78, 'Norland College Limited'),
			_Utils_Tuple2(79, 'Norwich University of the Arts'),
			_Utils_Tuple2(80, 'Nottingham Trent University'),
			_Utils_Tuple2(81, 'Osteopathic Education and Research Limited'),
			_Utils_Tuple2(82, 'Oxford Brookes University'),
			_Utils_Tuple2(83, 'Pearson College Limited'),
			_Utils_Tuple2(84, 'Plymouth College of Art'),
			_Utils_Tuple2(85, 'Point Blank Limited'),
			_Utils_Tuple2(86, 'QAHE (UR) Limited'),
			_Utils_Tuple2(87, 'Queen Margaret University, Edinburgh'),
			_Utils_Tuple2(88, 'Queen Mary University of London'),
			_Utils_Tuple2(89, 'Queen\'s University of Belfast'),
			_Utils_Tuple2(90, 'Ravensbourne University London'),
			_Utils_Tuple2(91, 'Regent\'s University London'),
			_Utils_Tuple2(92, 'Richmond, the American International University in London, Inc.'),
			_Utils_Tuple2(93, 'Robert Gordon University'),
			_Utils_Tuple2(94, 'Roehampton University'),
			_Utils_Tuple2(95, 'Rose Bruford College of Theatre and Performance'),
			_Utils_Tuple2(96, 'Royal College of Music'),
			_Utils_Tuple2(97, 'Royal Conservatoire of Scotland'),
			_Utils_Tuple2(98, 'Royal Holloway and Bedford New College'),
			_Utils_Tuple2(99, 'Royal Northern College of Music'),
			_Utils_Tuple2(100, 'Royal Welsh College of Music and Drama'),
			_Utils_Tuple2(101, 'SAE Education Limited'),
			_Utils_Tuple2(102, 'Sheffield Hallam University'),
			_Utils_Tuple2(103, 'Solent University'),
			_Utils_Tuple2(104, 'Spirit SSR Limited'),
			_Utils_Tuple2(105, 'St Mary\'s University College'),
			_Utils_Tuple2(106, 'St Mary\'s University, Twickenham'),
			_Utils_Tuple2(107, 'St Mellitus College Trust'),
			_Utils_Tuple2(108, 'St. George\'s Hospital Medical School'),
			_Utils_Tuple2(109, 'St. Piran\'s School (GB) Limited'),
			_Utils_Tuple2(110, 'Staffordshire University'),
			_Utils_Tuple2(111, 'Stranmillis University College'),
			_Utils_Tuple2(112, 'Swansea University'),
			_Utils_Tuple2(113, 'Teesside University'),
			_Utils_Tuple2(114, 'The College of Integrated Chinese Medicine'),
			_Utils_Tuple2(115, 'The Conservatoire for Dance and Drama'),
			_Utils_Tuple2(116, 'The Liverpool Institute for Performing Arts'),
			_Utils_Tuple2(117, 'The London Institute of Banking & Finance'),
			_Utils_Tuple2(118, 'The London School of Economics and Political Science'),
			_Utils_Tuple2(119, 'The Northern School of Art'),
			_Utils_Tuple2(120, 'The Open University'),
			_Utils_Tuple2(121, 'The Royal Academy of Music'),
			_Utils_Tuple2(122, 'The Royal Agricultural University'),
			_Utils_Tuple2(123, 'The Royal Veterinary College'),
			_Utils_Tuple2(124, 'The School of Oriental and African Studies'),
			_Utils_Tuple2(125, 'The University of Bath'),
			_Utils_Tuple2(126, 'The University of Birmingham'),
			_Utils_Tuple2(127, 'The University of Bolton'),
			_Utils_Tuple2(128, 'The University of Bradford'),
			_Utils_Tuple2(129, 'The University of Buckingham'),
			_Utils_Tuple2(130, 'The University of Chichester'),
			_Utils_Tuple2(131, 'The University of Cumbria'),
			_Utils_Tuple2(132, 'The University of East Anglia'),
			_Utils_Tuple2(133, 'The University of Essex'),
			_Utils_Tuple2(134, 'The University of Huddersfield'),
			_Utils_Tuple2(135, 'The University of Hull'),
			_Utils_Tuple2(136, 'The University of Kent'),
			_Utils_Tuple2(137, 'The University of Lancaster'),
			_Utils_Tuple2(138, 'The University of Law Limited'),
			_Utils_Tuple2(139, 'The University of Leeds'),
			_Utils_Tuple2(140, 'The University of Leicester'),
			_Utils_Tuple2(141, 'The University of Liverpool'),
			_Utils_Tuple2(142, 'The University of Manchester'),
			_Utils_Tuple2(143, 'The University of Reading'),
			_Utils_Tuple2(144, 'The University of Sheffield'),
			_Utils_Tuple2(145, 'The University of Surrey'),
			_Utils_Tuple2(146, 'The University of Warwick'),
			_Utils_Tuple2(147, 'The University of West London'),
			_Utils_Tuple2(148, 'The University of Westminster'),
			_Utils_Tuple2(149, 'Trinity Laban Conservatoire of Music and Dance'),
			_Utils_Tuple2(150, 'UCFB College of Football Business Limited'),
			_Utils_Tuple2(151, 'University College Birmingham'),
			_Utils_Tuple2(152, 'University College London'),
			_Utils_Tuple2(153, 'University College of Estate Management'),
			_Utils_Tuple2(154, 'University College of Osteopathy (The)'),
			_Utils_Tuple2(155, 'University for the Creative Arts'),
			_Utils_Tuple2(156, 'University of Aberdeen'),
			_Utils_Tuple2(157, 'University of Bedfordshire'),
			_Utils_Tuple2(158, 'University of Brighton'),
			_Utils_Tuple2(159, 'University of Bristol'),
			_Utils_Tuple2(160, 'University of Cambridge'),
			_Utils_Tuple2(161, 'University of Central Lancashire'),
			_Utils_Tuple2(162, 'University of Chester'),
			_Utils_Tuple2(163, 'University of Derby'),
			_Utils_Tuple2(164, 'University of Dundee'),
			_Utils_Tuple2(165, 'University of Durham'),
			_Utils_Tuple2(166, 'University of East London'),
			_Utils_Tuple2(167, 'University of Edinburgh'),
			_Utils_Tuple2(168, 'University of Exeter'),
			_Utils_Tuple2(169, 'University of Glasgow'),
			_Utils_Tuple2(170, 'University of Gloucestershire'),
			_Utils_Tuple2(171, 'University of Greenwich'),
			_Utils_Tuple2(172, 'University of Hertfordshire'),
			_Utils_Tuple2(173, 'University of Keele'),
			_Utils_Tuple2(174, 'University of Lincoln'),
			_Utils_Tuple2(175, 'University of Newcastle upon Tyne'),
			_Utils_Tuple2(176, 'University of Northampton, The'),
			_Utils_Tuple2(177, 'University of Northumbria at Newcastle'),
			_Utils_Tuple2(178, 'University of Nottingham, The'),
			_Utils_Tuple2(179, 'University of Plymouth'),
			_Utils_Tuple2(180, 'University of Portsmouth'),
			_Utils_Tuple2(181, 'University of Salford, The'),
			_Utils_Tuple2(182, 'University of South Wales'),
			_Utils_Tuple2(183, 'University of Southampton'),
			_Utils_Tuple2(184, 'University of St Andrews'),
			_Utils_Tuple2(185, 'University of St Mark & St John'),
			_Utils_Tuple2(186, 'University of Stirling'),
			_Utils_Tuple2(187, 'University of Strathclyde'),
			_Utils_Tuple2(188, 'University of Suffolk'),
			_Utils_Tuple2(189, 'University of Sunderland'),
			_Utils_Tuple2(190, 'University of Sussex'),
			_Utils_Tuple2(191, 'University of Ulster'),
			_Utils_Tuple2(192, 'University of Wales Trinity Saint David'),
			_Utils_Tuple2(193, 'University of Winchester'),
			_Utils_Tuple2(194, 'University of Wolverhampton'),
			_Utils_Tuple2(195, 'University of Worcester'),
			_Utils_Tuple2(196, 'University of York'),
			_Utils_Tuple2(197, 'University of the Arts, London'),
			_Utils_Tuple2(198, 'University of the Highlands and Islands'),
			_Utils_Tuple2(199, 'University of the West of England, Bristol'),
			_Utils_Tuple2(200, 'University of the West of Scotland'),
			_Utils_Tuple2(201, 'Urdang Schools Limited'),
			_Utils_Tuple2(202, 'Warwickshire College'),
			_Utils_Tuple2(203, 'Wrexham Glyndŵr University'),
			_Utils_Tuple2(204, 'Writtle University College'),
			_Utils_Tuple2(205, 'York St John University')
		]));
var $author$project$Data$unisOffering = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			1,
			_List_fromArray(
				[7, 12, 14, 16, 17, 21, 24, 26, 27, 28, 29, 31, 33, 37, 42, 49, 56, 60, 64, 65, 67, 70, 71, 74, 80, 82, 88, 89, 93, 98, 102, 112, 118, 120, 125, 126, 128, 132, 133, 134, 135, 136, 137, 139, 141, 142, 143, 144, 145, 146, 147, 148, 156, 157, 158, 159, 161, 162, 163, 164, 165, 166, 167, 168, 169, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 186, 187, 189, 190, 191, 194, 199, 200])),
			_Utils_Tuple2(
			2,
			_List_fromArray(
				[7, 14, 17, 21, 25, 26, 29, 31, 33, 36, 37, 42, 55, 56, 60, 64, 67, 71, 74, 82, 89, 93, 102, 110, 112, 113, 120, 127, 128, 131, 132, 133, 134, 135, 139, 142, 145, 147, 157, 158, 161, 162, 163, 164, 166, 170, 171, 172, 173, 174, 176, 177, 178, 179, 180, 181, 183, 186, 188, 189, 194, 195, 196, 199, 200])),
			_Utils_Tuple2(
			3,
			_List_fromArray(
				[24, 31, 52, 56, 70, 88, 89, 102, 112, 125, 139, 140, 141, 142, 144, 145, 158, 159, 169, 172, 181, 182, 183, 199])),
			_Utils_Tuple2(
			4,
			_List_fromArray(
				[124, 184])),
			_Utils_Tuple2(
			5,
			_List_fromArray(
				[46, 122, 175])),
			_Utils_Tuple2(
			6,
			_List_fromArray(
				[112, 132, 133, 135, 142, 178, 190])),
			_Utils_Tuple2(
			7,
			_List_fromArray(
				[31, 55, 72, 82, 89, 112, 134, 139, 140, 141, 143, 152, 156, 159, 164, 167, 169, 178, 179])),
			_Utils_Tuple2(
			8,
			_List_fromArray(
				[46, 47, 80, 122, 123, 162, 174, 204])),
			_Utils_Tuple2(
			9,
			_List_fromArray(
				[21, 44, 82, 118, 124, 136, 142, 152, 156, 159, 165, 167, 184, 190])),
			_Utils_Tuple2(
			10,
			_List_fromArray(
				[21, 28, 31, 42, 62, 64, 70, 158, 165])),
			_Utils_Tuple2(
			11,
			_List_fromArray(
				[140, 141, 144, 165, 175, 183, 193, 196])),
			_Utils_Tuple2(
			12,
			_List_fromArray(
				[8, 17, 28, 31, 33, 56, 60, 64, 65, 67, 71, 80, 82, 90, 93, 102, 125, 134, 136, 141, 143, 148, 152, 155, 158, 164, 166, 167, 171, 172, 175, 177, 178, 179, 180, 181, 187, 191, 197, 199])),
			_Utils_Tuple2(
			13,
			_List_fromArray(
				[6, 11, 15, 17, 27, 31, 33, 39, 43, 44, 56, 59, 60, 64, 70, 71, 79, 80, 93, 137, 139, 143, 148, 152, 155, 158, 164, 167, 174, 175, 177, 183, 191, 197, 199])),
			_Utils_Tuple2(
			14,
			_List_fromArray(
				[167, 178, 190])),
			_Utils_Tuple2(
			15,
			_List_fromArray(
				[16, 124, 139, 142, 144, 161, 167])),
			_Utils_Tuple2(
			16,
			_List_fromArray(
				[136, 169, 180])),
			_Utils_Tuple2(
			17,
			_List_fromArray(
				[55, 128, 144, 187])),
			_Utils_Tuple2(
			18,
			_List_fromArray(
				[37, 49, 52, 56, 64, 71, 80, 82, 88, 89, 102, 112, 120, 125, 126, 132, 134, 135, 136, 137, 139, 141, 142, 143, 144, 146, 148, 152, 159, 161, 162, 165, 167, 168, 173, 175, 177, 178, 182, 183, 189, 190, 194, 196])),
			_Utils_Tuple2(
			19,
			_List_fromArray(
				[28, 55, 74, 139, 142, 152, 156, 159, 169, 173, 178, 190])),
			_Utils_Tuple2(
			20,
			_List_fromArray(
				[94])),
			_Utils_Tuple2(
			21,
			_List_fromArray(
				[7, 17, 31, 37, 42, 49, 60, 64, 67, 70, 80, 82, 102, 143, 148, 153, 161, 171, 177, 178, 179, 180, 181, 182, 191, 194, 199])),
			_Utils_Tuple2(
			22,
			_List_fromArray(
				[9])),
			_Utils_Tuple2(
			23,
			_List_fromArray(
				[5, 6, 7, 9, 12, 14, 15, 16, 17, 20, 21, 24, 26, 27, 28, 29, 31, 33, 36, 37, 39, 42, 52, 53, 54, 56, 60, 62, 63, 64, 65, 67, 70, 71, 74, 80, 83, 86, 87, 88, 91, 93, 94, 98, 102, 103, 106, 109, 110, 112, 113, 125, 126, 128, 129, 132, 134, 135, 136, 137, 139, 141, 142, 143, 146, 147, 148, 151, 152, 155, 157, 158, 162, 164, 165, 167, 169, 171, 172, 174, 176, 177, 178, 179, 180, 181, 182, 183, 184, 186, 187, 188, 189, 190, 191, 194, 195, 199, 200, 203])),
			_Utils_Tuple2(
			24,
			_List_fromArray(
				[12, 49, 52, 67, 70, 89, 112, 125, 126, 135, 139, 142, 144, 145, 152, 156, 167, 175, 178, 187])),
			_Utils_Tuple2(
			25,
			_List_fromArray(
				[28, 49, 52, 55, 56, 70, 71, 80, 88, 125, 126, 134, 135, 136, 139, 140, 141, 142, 143, 144, 145, 146, 152, 159, 165, 167, 169, 173, 174, 175, 177, 178, 183, 184, 187, 190, 196])),
			_Utils_Tuple2(
			26,
			_List_fromArray(
				[31, 134, 191, 194, 200])),
			_Utils_Tuple2(
			27,
			_List_fromArray(
				[7, 17, 25, 26, 29, 36, 37, 42, 55, 56, 67, 74, 89, 102, 113, 139, 142, 145, 161, 171, 172, 177, 179, 181, 183, 199])),
			_Utils_Tuple2(
			28,
			_List_fromArray(
				[6, 7, 11, 17, 21, 24, 26, 31, 33, 36, 37, 39, 56, 59, 60, 64, 67, 71, 73, 74, 79, 80, 84, 90, 94, 98, 101, 102, 103, 110, 127, 133, 134, 136, 143, 148, 155, 158, 159, 161, 163, 169, 170, 172, 177, 178, 179, 180, 181, 182, 189, 191, 192, 196, 197, 199])),
			_Utils_Tuple2(
			29,
			_List_fromArray(
				[24, 28, 29, 31, 42, 49, 52, 56, 60, 64, 67, 70, 80, 89, 112, 113, 120, 125, 126, 128, 139, 141, 142, 144, 145, 147, 152, 153, 158, 159, 166, 167, 168, 169, 175, 177, 178, 179, 180, 183, 187, 191, 200])),
			_Utils_Tuple2(
			30,
			_List_fromArray(
				[55, 98, 112, 120, 126, 136, 139, 142, 143, 152, 165, 167, 168, 175, 178])),
			_Utils_Tuple2(
			31,
			_List_fromArray(
				[120, 125, 141, 168, 172, 175])),
			_Utils_Tuple2(
			32,
			_List_fromArray(
				[3, 81, 114, 134, 154, 181, 182])),
			_Utils_Tuple2(
			33,
			_List_fromArray(
				[5, 7, 17, 21, 24, 33, 39, 71, 83, 101, 103, 110, 127, 135, 170, 174])),
			_Utils_Tuple2(
			34,
			_List_fromArray(
				[5, 6, 7, 9, 12, 14, 15, 16, 17, 21, 24, 25, 26, 27, 28, 29, 31, 33, 36, 37, 42, 44, 49, 52, 55, 60, 64, 65, 67, 70, 71, 74, 80, 82, 86, 88, 89, 93, 98, 102, 110, 112, 113, 125, 126, 128, 132, 133, 134, 135, 136, 137, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 152, 157, 158, 159, 161, 163, 164, 165, 166, 167, 168, 169, 171, 172, 173, 174, 175, 177, 178, 179, 180, 181, 182, 183, 184, 186, 187, 189, 190, 191, 194, 196, 199, 200])),
			_Utils_Tuple2(
			35,
			_List_fromArray(
				[24, 26, 28, 31, 42, 67, 71, 77, 102, 120, 128, 131, 134, 141, 161, 163, 176, 177, 179, 181, 182, 191, 194, 195, 199, 205])),
			_Utils_Tuple2(
			36,
			_List_fromArray(
				[6, 14, 15, 39, 64, 71, 94, 102, 120, 126, 132, 135, 170, 180, 181, 193])),
			_Utils_Tuple2(
			37,
			_List_fromArray(
				[35, 68, 74, 94, 115, 130, 149, 166])),
			_Utils_Tuple2(
			38,
			_List_fromArray(
				[28, 55, 88, 89, 126, 139, 141, 142, 144, 159, 164, 169, 175, 179])),
			_Utils_Tuple2(
			39,
			_List_fromArray(
				[5, 7, 11, 15, 17, 19, 21, 24, 25, 27, 31, 33, 37, 39, 40, 41, 42, 43, 44, 48, 49, 53, 56, 59, 60, 64, 65, 67, 70, 71, 74, 79, 80, 84, 90, 93, 102, 103, 110, 113, 119, 120, 134, 136, 139, 142, 143, 147, 148, 155, 158, 161, 162, 163, 164, 166, 167, 169, 170, 171, 172, 174, 176, 177, 179, 180, 181, 182, 183, 189, 191, 192, 194, 197, 199, 205])),
			_Utils_Tuple2(
			40,
			_List_fromArray(
				[55, 124, 146, 190])),
			_Utils_Tuple2(
			41,
			_List_fromArray(
				[27, 74])),
			_Utils_Tuple2(
			42,
			_List_fromArray(
				[4, 6, 7, 11, 15, 17, 24, 33, 36, 39, 44, 45, 58, 63, 64, 67, 76, 80, 87, 88, 94, 95, 97, 98, 100, 106, 110, 116, 126, 130, 132, 133, 134, 136, 139, 143, 145, 146, 147, 151, 155, 159, 161, 168, 169, 170, 174, 176, 179, 181, 182, 190, 193, 196, 197, 199, 201, 205])),
			_Utils_Tuple2(
			43,
			_List_fromArray(
				[28, 52, 98, 126, 132, 135, 139, 140, 141, 152, 156, 159, 165, 167, 179, 180, 183])),
			_Utils_Tuple2(
			44,
			_List_fromArray(
				[6, 14, 135, 167, 168, 179, 199])),
			_Utils_Tuple2(
			45,
			_List_fromArray(
				[12, 16, 17, 24, 28, 29, 31, 33, 44, 49, 55, 56, 60, 70, 71, 74, 80, 82, 88, 89, 98, 102, 112, 118, 120, 124, 125, 126, 129, 132, 133, 135, 136, 137, 139, 140, 141, 142, 143, 144, 145, 146, 152, 156, 159, 164, 165, 167, 168, 169, 171, 172, 173, 175, 178, 179, 180, 183, 184, 187, 190, 191, 196, 199])),
			_Utils_Tuple2(
			46,
			_List_fromArray(
				[6, 7, 14, 15, 17, 18, 22, 24, 26, 27, 33, 36, 44, 60, 62, 63, 64, 65, 67, 71, 74, 77, 78, 80, 82, 94, 102, 110, 111, 113, 120, 126, 131, 132, 134, 135, 139, 144, 151, 157, 158, 159, 162, 163, 164, 165, 166, 170, 171, 172, 176, 179, 180, 187, 192, 193, 194, 195, 196, 198, 199, 203, 205])),
			_Utils_Tuple2(
			47,
			_List_fromArray(
				[12, 17, 24, 31, 42, 49, 52, 55, 64, 67, 70, 71, 88, 89, 102, 112, 113, 125, 126, 134, 136, 139, 141, 142, 144, 145, 146, 148, 152, 159, 167, 169, 175, 177, 178, 179, 180, 183, 187, 190, 196, 199])),
			_Utils_Tuple2(
			48,
			_List_fromArray(
				[31, 37, 42, 52, 70, 120, 146, 165, 191, 194])),
			_Utils_Tuple2(
			49,
			_List_fromArray(
				[12, 28, 102, 112, 126, 134, 137, 139, 142, 144, 148, 158, 169, 180, 205])),
			_Utils_Tuple2(
			50,
			_List_fromArray(
				[17, 21, 24, 28, 31, 37, 44, 55, 64, 70, 71, 82, 88, 89, 98, 120, 126, 129, 135, 136, 137, 139, 140, 141, 143, 144, 146, 156, 159, 164, 168, 173, 174, 175, 177, 178, 179, 181, 184, 187, 189, 190, 191, 194, 196, 199])),
			_Utils_Tuple2(
			51,
			_List_fromArray(
				[142, 166])),
			_Utils_Tuple2(
			52,
			_List_fromArray(
				[71, 102, 132, 137, 142, 179])),
			_Utils_Tuple2(
			53,
			_List_fromArray(
				[6, 7, 14, 17, 21, 24, 28, 29, 31, 33, 42, 49, 67, 71, 74, 80, 88, 89, 91, 98, 102, 112, 117, 118, 125, 126, 132, 133, 134, 135, 136, 137, 139, 140, 141, 142, 143, 145, 146, 152, 156, 157, 158, 159, 165, 167, 169, 171, 172, 173, 174, 178, 180, 181, 183, 187, 190, 191, 199])),
			_Utils_Tuple2(
			54,
			_List_fromArray(
				[102])),
			_Utils_Tuple2(
			55,
			_List_fromArray(
				[151])),
			_Utils_Tuple2(
			56,
			_List_fromArray(
				[139, 143])),
			_Utils_Tuple2(
			57,
			_List_fromArray(
				[7, 21, 26, 28, 31, 33, 64, 110, 113, 128, 136, 147, 152, 161, 163, 174, 182])),
			_Utils_Tuple2(
			58,
			_List_fromArray(
				[28, 37, 55, 88, 89, 125, 136, 139, 141, 142, 143, 144, 146, 152, 159, 167, 169, 175, 178, 183, 187])),
			_Utils_Tuple2(
			59,
			_List_fromArray(
				[88, 112, 144, 156, 169, 196])),
			_Utils_Tuple2(
			60,
			_List_fromArray(
				[139, 142, 144, 146, 152, 159, 175])),
			_Utils_Tuple2(
			61,
			_List_fromArray(
				[7, 17, 21, 26, 27, 29, 31, 33, 42, 52, 55, 60, 64, 74, 82, 102, 106, 108, 113, 120, 126, 128, 132, 133, 137, 142, 144, 145, 146, 152, 158, 162, 165, 166, 168, 171, 172, 174, 175, 180, 181, 191, 195, 199, 200])),
			_Utils_Tuple2(
			62,
			_List_fromArray(
				[9, 62, 80, 147, 189, 191, 194, 198])),
			_Utils_Tuple2(
			63,
			_List_fromArray(
				[33])),
			_Utils_Tuple2(
			64,
			_List_fromArray(
				[6, 14, 15, 16, 24, 26, 28, 31, 33, 44, 55, 60, 64, 70, 71, 80, 82, 88, 89, 98, 102, 112, 118, 120, 124, 126, 130, 132, 133, 134, 135, 136, 137, 139, 140, 142, 143, 144, 146, 148, 152, 156, 162, 163, 164, 165, 167, 168, 169, 172, 173, 174, 175, 177, 178, 179, 180, 182, 183, 184, 186, 187, 190, 191, 193, 194, 196, 199, 205])),
			_Utils_Tuple2(
			65,
			_List_fromArray(
				[30, 44, 56, 139, 152, 167, 169, 184, 190, 196])),
			_Utils_Tuple2(
			66,
			_List_fromArray(
				[6, 28, 31, 55, 60, 70, 82, 88, 89, 98, 102, 112, 118, 126, 132, 137, 139, 140, 141, 142, 143, 144, 152, 156, 165, 167, 168, 169, 173, 175, 177, 178, 183, 184, 190, 195, 199])),
			_Utils_Tuple2(
			67,
			_List_fromArray(
				[7, 25, 31, 33, 64, 71, 80, 102, 180, 187, 191])),
			_Utils_Tuple2(
			68,
			_List_fromArray(
				[120, 139])),
			_Utils_Tuple2(
			69,
			_List_fromArray(
				[28, 55, 80, 89, 91, 125, 139, 141, 142, 143, 144, 148, 152, 159, 162, 167, 169, 175, 178, 183, 184, 187])),
			_Utils_Tuple2(
			70,
			_List_fromArray(
				[150, 178, 191])),
			_Utils_Tuple2(
			71,
			_List_fromArray(
				[17, 33, 37, 56, 60, 65, 71, 74, 89, 102, 134, 171, 177, 180, 182, 191, 199, 200])),
			_Utils_Tuple2(
			72,
			_List_fromArray(
				[159])),
			_Utils_Tuple2(
			73,
			_List_fromArray(
				[13, 21, 28, 29, 33, 37, 60, 64, 80, 94, 102, 103, 110, 134, 144, 148, 158, 161, 163, 170, 174, 177, 180, 181, 187, 197, 199])),
			_Utils_Tuple2(
			74,
			_List_fromArray(
				[144])),
			_Utils_Tuple2(
			75,
			_List_fromArray(
				[6, 7, 9, 12, 14, 16, 17, 21, 24, 26, 28, 29, 31, 33, 36, 37, 54, 55, 56, 60, 64, 65, 67, 71, 74, 80, 82, 88, 89, 93, 94, 98, 102, 106, 110, 112, 113, 118, 120, 124, 126, 128, 129, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 152, 156, 157, 158, 159, 161, 162, 163, 164, 165, 166, 167, 168, 169, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 186, 187, 189, 190, 191, 193, 194, 195, 196, 199, 200])),
			_Utils_Tuple2(
			76,
			_List_fromArray(
				[42, 113, 191])),
			_Utils_Tuple2(
			77,
			_List_fromArray(
				[71, 80, 88, 136, 137, 139, 142, 146, 152, 165, 167, 196])),
			_Utils_Tuple2(
			78,
			_List_fromArray(
				[6, 7, 14, 15, 28, 60, 80, 94, 102, 112, 120, 132, 133, 134, 139, 141, 142, 144, 145, 146, 148, 152, 158, 162, 165, 167, 169, 175, 183, 186, 193, 195, 205])),
			_Utils_Tuple2(
			79,
			_List_fromArray(
				[6, 7, 9, 16, 17, 21, 29, 31, 33, 36, 37, 42, 44, 49, 55, 60, 65, 70, 71, 74, 80, 82, 88, 89, 91, 93, 98, 102, 103, 118, 124, 125, 126, 132, 133, 134, 135, 136, 139, 140, 141, 142, 143, 144, 145, 146, 148, 152, 153, 156, 159, 161, 163, 165, 166, 167, 168, 169, 170, 171, 173, 174, 175, 176, 178, 179, 180, 181, 183, 190, 191, 192, 193, 194, 196, 197, 199, 205])),
			_Utils_Tuple2(
			80,
			_List_fromArray(
				[175])),
			_Utils_Tuple2(
			81,
			_List_fromArray(
				[7, 12, 17, 21, 26, 27, 31, 33, 37, 39, 40, 42, 44, 49, 60, 62, 64, 65, 67, 71, 74, 80, 82, 88, 90, 91, 98, 102, 103, 125, 126, 132, 133, 134, 135, 136, 137, 139, 140, 141, 142, 143, 148, 151, 155, 158, 162, 163, 165, 168, 170, 171, 172, 173, 174, 175, 176, 179, 180, 182, 186, 187, 190, 191, 193, 197, 199, 200])),
			_Utils_Tuple2(
			82,
			_List_fromArray(
				[142])),
			_Utils_Tuple2(
			83,
			_List_fromArray(
				[52, 70, 112, 126, 144])),
			_Utils_Tuple2(
			84,
			_List_fromArray(
				[12, 24, 28, 29, 31, 49, 52, 55, 56, 64, 70, 71, 80, 88, 89, 98, 102, 112, 118, 120, 125, 126, 132, 133, 136, 137, 139, 140, 141, 142, 143, 144, 145, 146, 152, 159, 164, 165, 167, 168, 169, 171, 172, 173, 175, 178, 179, 180, 183, 184, 187, 190, 196])),
			_Utils_Tuple2(
			85,
			_List_fromArray(
				[12, 17, 24, 28, 29, 31, 33, 42, 46, 49, 52, 56, 64, 67, 70, 71, 82, 88, 89, 93, 102, 103, 110, 112, 113, 125, 126, 127, 134, 135, 137, 139, 140, 141, 142, 144, 145, 146, 152, 156, 158, 159, 161, 163, 164, 167, 168, 169, 172, 174, 175, 177, 178, 179, 180, 181, 182, 183, 187, 190, 191, 192, 199])),
			_Utils_Tuple2(
			86,
			_List_fromArray(
				[6, 7, 14, 15, 16, 17, 21, 24, 26, 28, 31, 33, 36, 37, 39, 42, 44, 55, 56, 60, 62, 64, 65, 70, 71, 74, 80, 82, 87, 88, 90, 98, 101, 102, 103, 110, 112, 130, 132, 134, 137, 139, 140, 141, 147, 148, 158, 161, 163, 166, 170, 171, 174, 175, 178, 180, 181, 182, 183, 184, 185, 186, 189, 190, 191, 193, 195, 197, 199, 200, 205])),
			_Utils_Tuple2(
			87,
			_List_fromArray(
				[17, 26, 28, 29, 42, 67, 102, 108, 113, 131, 139, 141, 168, 172, 180, 181, 188, 191, 199])),
			_Utils_Tuple2(
			88,
			_List_fromArray(
				[23, 28, 50, 52, 55, 88, 89, 108, 112, 126, 129, 132, 137, 139, 140, 141, 142, 144, 146, 152, 156, 159, 160, 161, 164, 167, 168, 169, 173, 175, 178, 179, 183, 184])),
			_Utils_Tuple2(
			89,
			_List_fromArray(
				[7, 14, 17, 25, 33, 36, 37, 42, 64, 67, 74, 102, 112, 113, 120, 133, 134, 142, 147, 157, 161, 162, 163, 171, 172, 176, 177, 179, 181, 182, 188, 194, 199, 200])),
			_Utils_Tuple2(
			90,
			_List_fromArray(
				[37, 132, 142, 167, 169, 187, 196])),
			_Utils_Tuple2(
			91,
			_List_fromArray(
				[7, 17, 21, 26, 29, 33, 37, 55, 56, 67, 74, 89, 93, 102, 134, 139, 142, 145, 147, 157, 158, 171, 172, 177, 178, 179, 181, 194, 199, 200])),
			_Utils_Tuple2(
			92,
			_List_fromArray(
				[7, 17, 24, 28, 52, 55, 82, 88, 98, 112, 125, 126, 132, 134, 139, 140, 141, 142, 144, 145, 146, 152, 159, 167, 169, 171, 178, 180, 183, 190, 194, 196])),
			_Utils_Tuple2(
			93,
			_List_fromArray(
				[1, 2, 10, 13, 14, 15, 17, 26, 28, 34, 35, 36, 37, 39, 41, 44, 45, 51, 55, 57, 60, 61, 74, 85, 89, 96, 97, 98, 99, 100, 101, 103, 104, 116, 121, 126, 130, 134, 135, 136, 139, 141, 142, 145, 147, 148, 149, 159, 165, 169, 172, 175, 181, 182, 194, 196, 198, 200, 205])),
			_Utils_Tuple2(
			94,
			_List_fromArray(
				[187])),
			_Utils_Tuple2(
			95,
			_List_fromArray(
				[28, 126, 134, 141, 147, 163, 170, 180, 182, 198])),
			_Utils_Tuple2(
			96,
			_List_fromArray(
				[27, 31, 60, 65, 71, 102, 139, 145, 178, 179, 191])),
			_Utils_Tuple2(
			97,
			_List_fromArray(
				[146])),
			_Utils_Tuple2(
			98,
			_List_fromArray(
				[7, 12, 28, 29, 42, 128, 142, 179])),
			_Utils_Tuple2(
			99,
			_List_fromArray(
				[191])),
			_Utils_Tuple2(
			100,
			_List_fromArray(
				[12, 21, 27, 28, 42, 65, 71, 80, 102, 113, 120, 125, 128, 141, 142, 150, 156, 158, 164, 165, 183, 199, 200])),
			_Utils_Tuple2(
			101,
			_List_fromArray(
				[47, 150, 159, 165, 187])),
			_Utils_Tuple2(
			102,
			_List_fromArray(
				[86, 147])),
			_Utils_Tuple2(
			103,
			_List_fromArray(
				[15, 17, 120, 127, 134, 148, 155])),
			_Utils_Tuple2(
			104,
			_List_fromArray(
				[52])),
			_Utils_Tuple2(
			105,
			_List_fromArray(
				[16, 49, 55, 88, 102, 120, 126, 132, 133, 136, 140, 142, 144, 146, 152, 165, 167, 168, 171, 178, 180, 187, 190])),
			_Utils_Tuple2(
			106,
			_List_fromArray(
				[7, 36, 191])),
			_Utils_Tuple2(
			107,
			_List_fromArray(
				[120, 133, 134, 171])),
			_Utils_Tuple2(
			108,
			_List_fromArray(
				[33, 60, 101, 103, 120, 134, 199])),
			_Utils_Tuple2(
			109,
			_List_fromArray(
				[46, 74])),
			_Utils_Tuple2(
			110,
			_List_fromArray(
				[56, 72, 134, 152, 156, 172, 175])),
			_Utils_Tuple2(
			111,
			_List_fromArray(
				[12, 28, 33, 55, 56, 64, 72, 89, 93, 125, 126, 128, 132, 134, 142, 143, 152, 158, 161, 172, 173, 175, 178, 180, 187, 189, 194])),
			_Utils_Tuple2(
			112,
			_List_fromArray(
				[16, 28, 55, 71, 80, 82, 89, 98, 118, 120, 126, 132, 133, 135, 136, 137, 139, 141, 142, 143, 144, 146, 152, 158, 165, 167, 168, 169, 175, 178, 183, 184, 190, 196])),
			_Utils_Tuple2(
			113,
			_List_fromArray(
				[6, 15, 28, 31, 36, 64, 70, 80, 89, 98, 102, 112, 120, 126, 132, 135, 139, 140, 141, 142, 143, 144, 156, 158, 159, 162, 165, 167, 168, 175, 177, 178, 179, 180, 183, 184, 190, 196, 199])),
			_Utils_Tuple2(
			114,
			_List_fromArray(
				[120, 137])),
			_Utils_Tuple2(
			115,
			_List_fromArray(
				[28, 52, 55, 70, 80, 88, 89, 112, 120, 125, 126, 135, 136, 137, 139, 140, 141, 142, 144, 145, 146, 152, 159, 165, 167, 168, 169, 175, 178, 180, 183, 184, 187, 190, 196])),
			_Utils_Tuple2(
			116,
			_List_fromArray(
				[24, 28, 31, 42, 71, 102, 108, 113, 126, 128, 134, 141, 158, 166, 173, 177, 178, 179, 181, 191, 199])),
			_Utils_Tuple2(
			117,
			_List_fromArray(
				[49, 82, 141, 144, 152, 175, 199])),
			_Utils_Tuple2(
			118,
			_List_fromArray(
				[6, 12, 16, 24, 26, 28, 29, 31, 33, 44, 55, 56, 60, 65, 70, 71, 74, 80, 82, 88, 89, 92, 98, 112, 118, 120, 124, 125, 126, 129, 132, 133, 134, 135, 136, 137, 139, 140, 142, 143, 144, 145, 146, 148, 152, 156, 159, 162, 164, 165, 166, 167, 168, 169, 171, 173, 174, 175, 177, 178, 179, 180, 183, 184, 186, 187, 190, 191, 196, 199, 200])),
			_Utils_Tuple2(
			119,
			_List_fromArray(
				[142])),
			_Utils_Tuple2(
			120,
			_List_fromArray(
				[17, 70, 139, 141, 144, 178, 187])),
			_Utils_Tuple2(
			121,
			_List_fromArray(
				[5, 6, 7, 9, 12, 14, 15, 16, 17, 24, 25, 26, 27, 29, 31, 33, 36, 37, 44, 49, 55, 56, 60, 63, 64, 65, 67, 70, 71, 74, 77, 80, 82, 87, 88, 89, 94, 98, 102, 103, 106, 110, 112, 113, 125, 126, 127, 128, 129, 132, 133, 134, 135, 136, 137, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 152, 156, 157, 158, 159, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 186, 187, 188, 189, 190, 191, 193, 194, 195, 196, 199, 200, 205])),
			_Utils_Tuple2(
			122,
			_List_fromArray(
				[190])),
			_Utils_Tuple2(
			123,
			_List_fromArray(
				[87, 148, 191, 197])),
			_Utils_Tuple2(
			124,
			_List_fromArray(
				[46, 122])),
			_Utils_Tuple2(
			125,
			_List_fromArray(
				[152])),
			_Utils_Tuple2(
			126,
			_List_fromArray(
				[31, 33, 36, 44, 55, 65, 74, 125, 126, 136, 139, 142, 144, 157, 158, 159, 161, 162, 169, 187, 191, 192, 194, 196])),
			_Utils_Tuple2(
			127,
			_List_fromArray(
				[63, 91, 105, 120, 139, 146, 152, 176])),
			_Utils_Tuple2(
			128,
			_List_fromArray(
				[7, 17, 31, 36, 42, 56, 60, 64, 71, 74, 77, 89, 93, 102, 103, 110, 113, 120, 126, 128, 131, 134, 135, 136, 139, 151, 161, 163, 166, 172, 177, 180, 181, 187, 190, 191, 193, 199, 200])),
			_Utils_Tuple2(
			129,
			_List_fromArray(
				[5, 6, 7, 12, 14, 15, 17, 21, 24, 25, 26, 28, 29, 31, 32, 33, 36, 37, 42, 44, 55, 56, 60, 62, 63, 64, 65, 67, 70, 71, 74, 80, 82, 87, 89, 93, 94, 98, 102, 103, 110, 112, 113, 118, 125, 126, 127, 128, 133, 134, 135, 136, 137, 139, 140, 141, 142, 144, 145, 146, 147, 148, 152, 156, 157, 158, 159, 161, 162, 163, 165, 166, 167, 168, 169, 170, 171, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 186, 188, 189, 190, 191, 193, 195, 196, 199, 200, 203, 205])),
			_Utils_Tuple2(
			130,
			_List_fromArray(
				[12, 33, 37, 42, 71, 89, 102, 112, 134, 141, 169, 191, 199])),
			_Utils_Tuple2(
			131,
			_List_fromArray(
				[5, 7, 14, 17, 21, 24, 25, 26, 27, 31, 32, 36, 37, 47, 56, 60, 62, 63, 64, 65, 69, 70, 71, 74, 80, 82, 94, 102, 103, 106, 110, 111, 112, 113, 125, 126, 127, 130, 131, 132, 133, 134, 135, 136, 139, 151, 157, 158, 161, 162, 163, 165, 166, 168, 170, 172, 174, 175, 176, 177, 180, 181, 182, 185, 186, 189, 191, 192, 193, 194, 195, 198, 200, 205])),
			_Utils_Tuple2(
			132,
			_List_fromArray(
				[12, 49, 52, 118, 120, 142, 146, 152, 167, 169, 187])),
			_Utils_Tuple2(
			133,
			_List_fromArray(
				[14, 17, 18, 22, 26, 36, 60, 62, 63, 64, 71, 77, 80, 82, 94, 102, 105, 106, 111, 130, 131, 135, 143, 152, 156, 157, 158, 162, 163, 164, 165, 167, 169, 170, 171, 172, 176, 177, 179, 182, 185, 186, 187, 189, 192, 193, 194, 195, 199, 200, 205])),
			_Utils_Tuple2(
			134,
			_List_fromArray(
				[28, 38, 55, 66, 75, 89, 107, 126, 139, 142, 159, 165, 167, 168, 178, 205])),
			_Utils_Tuple2(
			135,
			_List_fromArray(
				[7, 13, 21, 25, 27, 31, 37, 42, 47, 60, 64, 71, 74, 82, 87, 93, 102, 103, 134, 145, 147, 148, 151, 158, 161, 162, 163, 166, 167, 170, 171, 172, 174, 176, 179, 189, 191, 192, 200])),
			_Utils_Tuple2(
			136,
			_List_fromArray(
				[123, 141, 145, 159, 167, 169, 178])),
			_Utils_Tuple2(
			137,
			_List_fromArray(
				[6, 7, 14, 64, 112, 135, 143, 156, 159, 167, 168, 169, 175, 181, 190, 202]))
		]));
var $author$project$Update$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 6:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aR: function () {
								var _v1 = model.aR;
								if (_v1.$ === 2) {
									return $author$project$DataTypes$FoldedUp;
								} else {
									return $author$project$DataTypes$Uni('');
								}
							}(),
							bu: _List_Nil
						}),
					$elm$core$Platform$Cmd$none);
			case 7:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aR: function () {
								var _v2 = model.aR;
								if (!_v2.$) {
									return $author$project$DataTypes$FoldedUp;
								} else {
									return $author$project$DataTypes$Question;
								}
							}()
						}),
					$elm$core$Platform$Cmd$none);
			case 8:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aR: function () {
								var _v3 = model.aR;
								if (_v3.$ === 1) {
									return $author$project$DataTypes$FoldedUp;
								} else {
									return $author$project$DataTypes$Subject('');
								}
							}(),
							bn: _List_Nil
						}),
					$elm$core$Platform$Cmd$none);
			case 0:
				if (!msg.a) {
					var _v4 = msg.a;
					return $author$project$Update$freshModel(
						_Utils_update(
							model,
							{
								aN: 0,
								aR: $author$project$DataTypes$FoldedUp,
								bd: _List_fromArray(
									[27]),
								bv: function () {
									var _v5 = model.X;
									if (_v5.$ === 1) {
										if (!_v5.a.$) {
											var subj = _v5.a.a;
											var _v6 = A2($elm$core$Dict$get, subj, $author$project$Data$unisOffering);
											if (_v6.$ === 1) {
												return _List_Nil;
											} else {
												var unis = _v6.a;
												return unis;
											}
										} else {
											var _v7 = _v5.a;
											return $elm$core$Dict$keys($author$project$Data$uniCodes);
										}
									} else {
										return $elm$core$Dict$keys($author$project$Data$overallUniCodes);
									}
								}()
							}));
				} else {
					var _v8 = msg.a;
					return $author$project$Update$freshModel(
						_Utils_update(
							model,
							{
								aN: 1,
								aR: $author$project$DataTypes$Uni(''),
								bd: $elm$core$Dict$keys($author$project$DataTypes$questionCodes),
								bv: _List_Nil
							}));
				}
			case 17:
				return $author$project$Update$freshModel(
					function () {
						var _v9 = model.X;
						if (!_v9.$) {
							return _Utils_update(
								model,
								{
									bv: $elm$core$Dict$keys($author$project$Data$overallUniCodes)
								});
						} else {
							if (!_v9.a.$) {
								var subject = _v9.a.a;
								return _Utils_update(
									model,
									{
										bv: function () {
											var _v10 = A2($elm$core$Dict$get, subject, $author$project$Data$unisOffering);
											if (_v10.$ === 1) {
												return $elm$core$Dict$keys($author$project$Data$uniCodes);
											} else {
												var unis = _v10.a;
												return unis;
											}
										}()
									});
							} else {
								var _v11 = _v9.a;
								return _Utils_update(
									model,
									{
										bv: $elm$core$Dict$keys($author$project$Data$uniCodes)
									});
							}
						}
					}());
			case 14:
				return $author$project$Update$freshModel(
					_Utils_update(
						model,
						{bv: _List_Nil}));
			case 16:
				return $author$project$Update$freshModel(
					_Utils_update(
						model,
						{
							bd: $elm$core$Dict$keys($author$project$DataTypes$questionCodes)
						}));
			case 15:
				return $author$project$Update$freshModel(
					_Utils_update(
						model,
						{bd: _List_Nil}));
			case 4:
				var txt = msg.a;
				var _v12 = A2($author$project$Update$search4words, model.bt, txt);
				var searchResults = _v12.a;
				var newSearchIndex = _v12.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aR: $author$project$DataTypes$Uni(txt),
							bt: newSearchIndex,
							bu: searchResults
						}),
					$elm$core$Platform$Cmd$none);
			case 5:
				var txt = msg.a;
				var _v13 = A2($author$project$Update$search4words, model.bm, txt);
				var searchResults = _v13.a;
				var newSearchIndex = _v13.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aR: $author$project$DataTypes$Subject(txt),
							bm: newSearchIndex,
							bn: searchResults
						}),
					$elm$core$Platform$Cmd$none);
			case 9:
				var subject = msg.a;
				var _v14 = _Utils_Tuple2(
					A2($elm$core$Dict$get, subject, $author$project$Data$unisOffering),
					model.aN);
				if (_v14.a.$ === 1) {
					var _v15 = _v14.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								aX: $elm$core$Maybe$Just('Could not find subject in \'unisOffering\'' + ' dictionary.')
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					if (!_v14.b) {
						var unis = _v14.a.a;
						var _v16 = _v14.b;
						return $author$project$Update$freshModel(
							_Utils_update(
								model,
								{
									X: $author$project$DataTypes$BySubject(
										$elm$core$Maybe$Just(subject)),
									aR: $author$project$DataTypes$FoldedUp,
									bv: function () {
										var _v17 = model.bv;
										if (_v17.b && (!_v17.b.b)) {
											return model.bv;
										} else {
											return unis;
										}
									}()
								}));
					} else {
						var _v18 = _v14.b;
						return $author$project$Update$freshModel(
							_Utils_update(
								model,
								{
									X: $author$project$DataTypes$BySubject(
										$elm$core$Maybe$Just(subject)),
									aR: function () {
										var _v19 = model.bv;
										if (!_v19.b) {
											return $author$project$DataTypes$Uni('');
										} else {
											return $author$project$DataTypes$FoldedUp;
										}
									}(),
									bu: _List_Nil
								}));
					}
				}
			case 13:
				var uni = msg.a;
				var _v20 = A2($elm$core$Dict$get, uni, $author$project$Data$subjectsOffered);
				if (_v20.$ === 1) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								aX: $elm$core$Maybe$Just('Could not find university in \'subjectsOffered\'' + ' dictionary.')
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var subjects = _v20.a;
					return $author$project$Update$freshModel(
						_Utils_update(
							model,
							{
								aR: function () {
									var _v21 = model.X;
									if (!_v21.$) {
										return $author$project$DataTypes$FoldedUp;
									} else {
										if (_v21.a.$ === 1) {
											var _v22 = _v21.a;
											return $author$project$DataTypes$Subject('');
										} else {
											return $author$project$DataTypes$FoldedUp;
										}
									}
								}(),
								bn: _List_Nil,
								bv: _List_fromArray(
									[uni])
							}));
				}
			case 11:
				var question = msg.a;
				return $author$project$Update$freshModel(
					_Utils_update(
						model,
						{
							bd: A2($author$project$Update$calculateDataChoice, model.bd, question)
						}));
			case 12:
				var q = msg.a;
				return $author$project$Update$freshModel(
					_Utils_update(
						model,
						{
							aR: function () {
								var _v23 = model.X;
								if (!_v23.$) {
									return $author$project$DataTypes$FoldedUp;
								} else {
									if (_v23.a.$ === 1) {
										var _v24 = _v23.a;
										return $author$project$DataTypes$Subject('');
									} else {
										return $author$project$DataTypes$FoldedUp;
									}
								}
							}(),
							bd: _List_fromArray(
								[q])
						}));
			case 10:
				var uni = msg.a;
				return $author$project$Update$freshModel(
					_Utils_update(
						model,
						{
							bv: A2($author$project$Update$calculateDataChoice, model.bv, uni)
						}));
			case 1:
				return $author$project$Update$freshModel(
					function () {
						var _v25 = model.aN;
						if (!_v25) {
							return _Utils_update(
								model,
								{
									X: $author$project$DataTypes$Overall,
									aR: $author$project$DataTypes$FoldedUp,
									bd: _List_fromArray(
										[27]),
									bt: $author$project$Update$makeSearchIndex($author$project$Data$overallUniCodes),
									bv: $elm$core$Dict$keys($author$project$Data$overallUniCodes)
								});
						} else {
							return _Utils_update(
								model,
								{
									X: $author$project$DataTypes$Overall,
									aR: $author$project$DataTypes$Uni(''),
									bd: $elm$core$Dict$keys($author$project$DataTypes$questionCodes),
									bt: $author$project$Update$makeSearchIndex($author$project$Data$overallUniCodes),
									bv: _List_Nil
								});
						}
					}());
			case 2:
				return $author$project$Update$freshModel(
					_Utils_update(
						model,
						{
							X: $author$project$DataTypes$BySubject($elm$core$Maybe$Nothing),
							aR: $author$project$DataTypes$Subject(''),
							bt: $author$project$Update$makeSearchIndex($author$project$Data$uniCodes),
							bv: _List_Nil
						}));
			default:
				var dat = msg.a;
				if (dat.$ === 1) {
					var err = dat.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								a_: $elm$core$Maybe$Just(
									$author$project$Update$errToStr(err)),
								bc: false
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var error = dat.a.aV;
					var result = dat.a.bg;
					if (error === '') {
						var _v27 = $author$project$Update$data2Tuple(result);
						if (!_v27.$) {
							var goodresult = _v27.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{aS: goodresult, a_: $elm$core$Maybe$Nothing, bc: false}),
								$elm$core$Platform$Cmd$none);
						} else {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										aS: _List_Nil,
										a_: $elm$core$Maybe$Just('Couldn\'t convert data to tuples.'),
										bc: false
									}),
								$elm$core$Platform$Cmd$none);
						}
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a_: $elm$core$Maybe$Just(error),
									bc: false
								}),
							$elm$core$Platform$Cmd$none);
					}
				}
		}
	});
var $author$project$Chart$charWidth = 9;
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Chart$maxStringLength = function (strs) {
	var _v0 = $elm$core$List$maximum(
		A2($elm$core$List$map, $elm$core$String$length, strs));
	if (!_v0.$) {
		var a = _v0.a;
		return a;
	} else {
		return 0;
	}
};
var $elm$core$Basics$round = _Basics_round;
var $author$project$Chart$calcOffset = function (labels) {
	return $elm$core$Basics$round(
		$author$project$Chart$charWidth * $author$project$Chart$maxStringLength(labels));
};
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Chart$axisType2Codes = F2(
	function (axisType, _v0) {
		var unis = _v0.bv;
		var questions = _v0.bd;
		var chartMode = _v0.X;
		if (!axisType) {
			if (!chartMode.$) {
				return _Utils_Tuple2($author$project$Data$overallUniCodes, unis);
			} else {
				return _Utils_Tuple2($author$project$Data$uniCodes, unis);
			}
		} else {
			return _Utils_Tuple2($author$project$DataTypes$questionCodes, questions);
		}
	});
var $author$project$Chart$getCurrentLabels = F2(
	function (codes, selections) {
		return $elm_community$maybe_extra$Maybe$Extra$combine(
			A2(
				$elm$core$List$map,
				function (a) {
					return A2($elm$core$Dict$get, a, codes);
				},
				selections));
	});
var $author$project$Chart$maybe2list = function (maybeList) {
	if (maybeList.$ === 1) {
		return _List_Nil;
	} else {
		var a = maybeList.a;
		return a;
	}
};
var $author$project$Chart$labelList = F2(
	function (model, axisType) {
		return $author$project$Chart$maybe2list(
			function (_v0) {
				var a = _v0.a;
				var b = _v0.b;
				return A2($author$project$Chart$getCurrentLabels, a, b);
			}(
				A2($author$project$Chart$axisType2Codes, axisType, model)));
	});
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $author$project$DataTypes$plotScale = 3;
var $author$project$AxisLabels$gapOnLeft = $author$project$DataTypes$plotScale * 2;
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $author$project$DataTypes$scale = 23;
var $author$project$Chart$plotDot = F3(
	function (x, y, yoffset) {
		var yCoord = (($elm$core$Basics$round($author$project$DataTypes$scale) * (y + 1)) + yoffset) + 1;
		var xCoord = ($author$project$DataTypes$plotScale * x) + $author$project$AxisLabels$gapOnLeft;
		return A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx(
					$elm$core$String$fromInt(xCoord)),
					$elm$svg$Svg$Attributes$cy(
					$elm$core$String$fromInt(yCoord)),
					$elm$svg$Svg$Attributes$r(
					$elm$core$String$fromInt(3)),
					$elm$svg$Svg$Attributes$fill('black')
				]),
			_List_Nil);
	});
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $author$project$Chart$plotLine = F4(
	function (x1, x2, y, yoffset) {
		var ycoord = $elm$core$String$fromInt(
			(($elm$core$Basics$round($author$project$DataTypes$scale) * (y + 1)) + yoffset) + 1);
		return A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1(
					$elm$core$String$fromInt((x1 * $author$project$DataTypes$plotScale) + $author$project$AxisLabels$gapOnLeft)),
					$elm$svg$Svg$Attributes$y1(ycoord),
					$elm$svg$Svg$Attributes$x2(
					$elm$core$String$fromInt((x2 * $author$project$DataTypes$plotScale) + $author$project$AxisLabels$gapOnLeft)),
					$elm$svg$Svg$Attributes$y2(ycoord),
					$elm$svg$Svg$Attributes$class('errorBar')
				]),
			_List_Nil);
	});
var $author$project$Chart$plotRow = F3(
	function (yoffset, rowNum, _v0) {
		var minConf = _v0.a;
		var val = _v0.b;
		var maxConf = _v0.c;
		return _List_fromArray(
			[
				A3($author$project$Chart$plotDot, val, rowNum, yoffset),
				A4($author$project$Chart$plotLine, minConf, maxConf, rowNum, yoffset)
			]);
	});
var $elm$svg$Svg$Attributes$preserveAspectRatio = _VirtualDom_attribute('preserveAspectRatio');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $author$project$AxisLabels$Medium = 1;
var $author$project$AxisLabels$Thick = 2;
var $author$project$AxisLabels$Thin = 0;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$AxisLabels$oneXLabel = F2(
	function (yoffset, pos) {
		var xpos = (pos * $author$project$DataTypes$plotScale) + $author$project$AxisLabels$gapOnLeft;
		return A2(
			$elm$svg$Svg$text_,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromInt(xpos)),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromInt(yoffset + 2)),
					$elm$svg$Svg$Attributes$textAnchor('middle'),
					$elm$svg$Svg$Attributes$fill('black')
				]),
			_List_fromArray(
				[
					$elm$svg$Svg$text(
					$elm$core$String$fromInt(pos))
				]));
	});
var $author$project$AxisLabels$oneVerticalLine = F4(
	function (weight, yoffset, maxY, xPos) {
		var xCoord = $elm$core$String$fromInt((xPos * $author$project$DataTypes$plotScale) + $author$project$AxisLabels$gapOnLeft);
		return A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1(xCoord),
					$elm$svg$Svg$Attributes$x2(xCoord),
					$elm$svg$Svg$Attributes$y1(
					$elm$core$String$fromInt(
						yoffset + $elm$core$Basics$round($author$project$DataTypes$scale * 0.5))),
					$elm$svg$Svg$Attributes$y2(
					$elm$core$String$fromInt(
						(yoffset + ($elm$core$Basics$round($author$project$DataTypes$scale) * maxY)) + 15)),
					$elm$svg$Svg$Attributes$class(
					function () {
						switch (weight) {
							case 2:
								return 'thickline';
							case 1:
								return 'mediumline';
							default:
								return 'thinline';
						}
					}())
				]),
			_List_Nil);
	});
var $author$project$AxisLabels$verticalLines = F4(
	function (yoffset, maxY, weight, xpositions) {
		return A2(
			$elm$core$List$map,
			A3($author$project$AxisLabels$oneVerticalLine, weight, yoffset, maxY),
			xpositions);
	});
var $author$project$AxisLabels$xAxisLabels = F4(
	function (model, xoffset, yoffset, numYLabels) {
		var xPositions = _List_fromArray(
			[0, 20, 40, 60, 80, 100]);
		var grid = A2($author$project$AxisLabels$verticalLines, yoffset, numYLabels);
		return _Utils_ap(
			A2(
				grid,
				2,
				_List_fromArray(
					[0, 20, 40, 60, 80, 100])),
			_Utils_ap(
				A2(
					grid,
					1,
					_List_fromArray(
						[10, 30, 50, 70, 90])),
				_Utils_ap(
					A2(
						grid,
						0,
						_List_fromArray(
							[5, 15, 25, 35, 45, 55, 65, 75, 85, 95])),
					A2(
						$elm$core$List$map,
						$author$project$AxisLabels$oneXLabel(yoffset),
						xPositions))));
	});
var $author$project$AxisLabels$axisValues = F2(
	function (model, axisType) {
		if (!axisType) {
			var _v1 = _Utils_Tuple2(model.bv, model.X);
			if (!_v1.a.b) {
				if (_v1.b.$ === 1) {
					return $elm$core$Dict$keys($author$project$Data$uniCodes);
				} else {
					var _v2 = _v1.b;
					return $elm$core$Dict$keys($author$project$Data$overallUniCodes);
				}
			} else {
				var unis = _v1.a;
				return unis;
			}
		} else {
			var _v3 = model.bd;
			if (!_v3.b) {
				return $elm$core$Dict$keys($author$project$DataTypes$questionCodes);
			} else {
				var questions = _v3;
				return questions;
			}
		}
	});
var $author$project$AxisLabels$getLabel = F3(
	function (chartMode, axisType, code) {
		var _v0 = _Utils_Tuple2(chartMode, axisType);
		if (!_v0.b) {
			if (_v0.a.$ === 1) {
				var _v1 = _v0.b;
				return A2($elm$core$Dict$get, code, $author$project$Data$uniCodes);
			} else {
				var _v2 = _v0.a;
				var _v3 = _v0.b;
				return A2($elm$core$Dict$get, code, $author$project$Data$overallUniCodes);
			}
		} else {
			var _v4 = _v0.b;
			return A2($elm$core$Dict$get, code, $author$project$DataTypes$questionCodes);
		}
	});
var $author$project$AxisLabels$oneYLabel = F5(
	function (chartMode, axisType, yoffset, pos, labelID) {
		var ypos = ((pos * $elm$core$Basics$round($author$project$DataTypes$scale)) + yoffset) + 5;
		var _v0 = A3($author$project$AxisLabels$getLabel, chartMode, axisType, labelID);
		if (_v0.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var label = _v0.a;
			return $elm$core$Maybe$Just(
				A2(
					$elm$svg$Svg$text_,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromInt(((100 * $author$project$DataTypes$plotScale) + $author$project$AxisLabels$gapOnLeft) + 12)),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromInt(ypos)),
							$elm$svg$Svg$Attributes$textAnchor('left'),
							$elm$svg$Svg$Attributes$fill('black')
						]),
					_List_fromArray(
						[
							$elm$svg$Svg$text(label)
						])));
		}
	});
var $author$project$DataTypes$QuestionAxis = 1;
var $author$project$DataTypes$UniAxis = 0;
var $author$project$DataTypes$yAxisType = function (axesConfig) {
	if (!axesConfig) {
		return 0;
	} else {
		return 1;
	}
};
var $author$project$AxisLabels$yAxisLabels = F3(
	function (model, xoffset, yoffset) {
		var axisType = $author$project$DataTypes$yAxisType(model.aN);
		var yVals = A2($author$project$AxisLabels$axisValues, model, axisType);
		var yPositions = A2(
			$elm$core$List$range,
			1,
			$elm$core$List$length(yVals));
		return $elm_community$maybe_extra$Maybe$Extra$combine(
			A3(
				$elm$core$List$map2,
				A3($author$project$AxisLabels$oneYLabel, model.X, axisType, yoffset),
				yPositions,
				yVals));
	});
var $author$project$Chart$chart = function (model) {
	var yAxisTypel = $author$project$DataTypes$yAxisType(model.aN);
	var yLabelNames = A2($author$project$Chart$labelList, model, yAxisTypel);
	var widestYlabel = $author$project$Chart$calcOffset(yLabelNames);
	var widestXlabel = 12;
	var xLabels = A4(
		$author$project$AxisLabels$xAxisLabels,
		model,
		widestYlabel,
		widestXlabel,
		$elm$core$List$length(yLabelNames));
	var viewY = widestXlabel + ($elm$core$Basics$round($author$project$DataTypes$scale) * ($elm$core$List$length(yLabelNames) + 1));
	var viewX = (widestYlabel + ($author$project$DataTypes$plotScale * 100)) + 20;
	var points = $elm$core$List$concat(
		A2(
			$elm$core$List$indexedMap,
			$author$project$Chart$plotRow(widestXlabel),
			model.aS));
	var maybeYLabels = A3($author$project$AxisLabels$yAxisLabels, model, widestYlabel, widestXlabel);
	if (!maybeYLabels.$) {
		var yLabels = maybeYLabels.a;
		return _List_fromArray(
			[
				A2(
				$elm$svg$Svg$svg,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$viewBox(
						A2(
							$elm$core$String$join,
							' ',
							_List_fromArray(
								[
									'0 0',
									$elm$core$String$fromInt(viewX),
									$elm$core$String$fromInt(viewY)
								]))),
						$elm$svg$Svg$Attributes$preserveAspectRatio('xMidYMid meet'),
						$elm$svg$Svg$Attributes$width(
						$elm$core$String$fromFloat(0.27 * viewX) + 'mm')
					]),
				$elm$core$List$concat(
					_List_fromArray(
						[xLabels, yLabels, points])))
			]);
	} else {
		return _List_Nil;
	}
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$View$QuestionErr = 1;
var $author$project$View$SubjectErr = 3;
var $author$project$View$UniErr = 2;
var $author$project$View$dataCorrectLength = F4(
	function (axesConfig, data, unis, questions) {
		if (!axesConfig) {
			return _Utils_eq(
				$elm$core$List$length(data),
				$elm$core$List$length(unis));
		} else {
			return _Utils_eq(
				$elm$core$List$length(data),
				$elm$core$List$length(questions));
		}
	});
var $author$project$View$noSelectWords = function (axisType) {
	switch (axisType) {
		case 2:
			return 'universities';
		case 1:
			return 'questions';
		case 0:
			return 'answers';
		default:
			return 'subjects';
	}
};
var $author$project$View$noSelectErrorMsg = function (axisType) {
	return $elm$core$String$concat(
		_List_fromArray(
			[
				'No ',
				$author$project$View$noSelectWords(axisType),
				' selected.'
			]));
};
var $author$project$View$errorMsg = function (_v0) {
	var pageLoading = _v0.bc;
	var axesConfig = _v0.aN;
	var chartMode = _v0.X;
	var data = _v0.aS;
	var getRequestErr = _v0.a_;
	var unis = _v0.bv;
	var questions = _v0.bd;
	var _v1 = _Utils_Tuple3(
		_Utils_Tuple3(pageLoading, getRequestErr, unis),
		_Utils_Tuple3(chartMode, questions, data),
		A4($author$project$View$dataCorrectLength, axesConfig, data, unis, questions));
	if (_v1.a.a) {
		var _v2 = _v1.a;
		var _v3 = _v1.b;
		return $elm$core$Maybe$Just('Loading data.  Please wait...');
	} else {
		if (!_v1.a.b.$) {
			var _v4 = _v1.a;
			var _v5 = _v1.b;
			return $elm$core$Maybe$Just('Chart not available.  Check internet connection.');
		} else {
			if (!_v1.a.c.b) {
				var _v6 = _v1.a;
				var _v7 = _v1.b;
				return $elm$core$Maybe$Just(
					$author$project$View$noSelectErrorMsg(2));
			} else {
				if ((_v1.b.a.$ === 1) && (_v1.b.a.a.$ === 1)) {
					var _v8 = _v1.a;
					var _v9 = _v1.b;
					var _v10 = _v9.a.a;
					return $elm$core$Maybe$Just(
						$author$project$View$noSelectErrorMsg(3));
				} else {
					if (!_v1.b.b.b) {
						var _v11 = _v1.a;
						var _v12 = _v1.b;
						return $elm$core$Maybe$Just(
							$author$project$View$noSelectErrorMsg(1));
					} else {
						if (!_v1.b.c.b) {
							var _v13 = _v1.a;
							var _v14 = _v1.b;
							return $elm$core$Maybe$Just('No chart because no data for this university and subject.');
						} else {
							if (!_v1.c) {
								var _v15 = _v1.a;
								var _v16 = _v1.b;
								return $elm$core$Maybe$Just('No chart because not enough data for these universities and subjects. Check to see if a university or subject in red is selected.');
							} else {
								return $elm$core$Maybe$Nothing;
							}
						}
					}
				}
			}
		}
	}
};
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$View$chartOrMsg = function (model) {
	var _v0 = _Utils_Tuple2(
		$author$project$View$errorMsg(model),
		model.aX);
	if (_v0.b.$ === 1) {
		if (!_v0.a.$) {
			var msg = _v0.a.a;
			var _v1 = _v0.b;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('errMsg')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(msg)
					]));
		} else {
			var _v2 = _v0.a;
			var _v3 = _v0.b;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				$author$project$Chart$chart(model));
		}
	} else {
		var err = _v0.b.a;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(
					$elm$core$String$concat(
						_List_fromArray(
							['There was an internal error, so the chart cannot be displayed: ', err])))
				]));
	}
};
var $author$project$DataTypes$ChooseAQuestion = {$: 7};
var $author$project$DataTypes$ChooseASubject = {$: 8};
var $author$project$DataTypes$ChooseAUni = {$: 6};
var $author$project$DataTypes$OverallMode = {$: 1};
var $author$project$DataTypes$SubjectMode = {$: 2};
var $author$project$DataChooser$Tick = 1;
var $author$project$DataChooser$axisMessagesAndLabels = _List_fromArray(
	[
		_Utils_Tuple2(0, 'Compare universities'),
		_Utils_Tuple2(1, 'Look at one university')
	]);
var $author$project$DataChooser$boxType2String = function (boxType) {
	if (!boxType) {
		return 'radio';
	} else {
		return 'checkbox';
	}
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$svg$Svg$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$DataChooser$checkBox = F5(
	function (checkBoxType, msg, label, ticked, enabled) {
		return A2(
			$elm$html$Html$label,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_(
							$author$project$DataChooser$boxType2String(checkBoxType)),
							$elm$svg$Svg$Events$onClick(msg),
							$elm$html$Html$Attributes$checked(ticked)
						]),
					_List_Nil),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							enabled ? 'blackCheckBox' : 'redCheckBox'),
							$elm$html$Html$Attributes$class('generalCheckBox')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						]))
				]));
	});
var $author$project$DataTypes$AxesConfigChoice = function (a) {
	return {$: 0, a: a};
};
var $author$project$DataChooser$Radio = 0;
var $author$project$DataChooser$radioButton = F2(
	function (alreadyChosen, _v0) {
		var thisOne = _v0.a;
		var label = _v0.b;
		return A5(
			$author$project$DataChooser$checkBox,
			0,
			$author$project$DataTypes$AxesConfigChoice(thisOne),
			label,
			_Utils_eq(alreadyChosen, thisOne),
			true);
	});
var $author$project$DataChooser$axesConfigMode = F2(
	function (alreadyChosen, chartMode) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('mainChoices')
				]),
			$elm$core$List$concat(
				_List_fromArray(
					[
						A2(
						$elm$core$List$map,
						$author$project$DataChooser$radioButton(alreadyChosen),
						$author$project$DataChooser$axisMessagesAndLabels),
						_List_fromArray(
						[
							A5(
							$author$project$DataChooser$checkBox,
							1,
							function () {
								if (!chartMode.$) {
									return $author$project$DataTypes$SubjectMode;
								} else {
									return $author$project$DataTypes$OverallMode;
								}
							}(),
							'Compare by subject area',
							!_Utils_eq(chartMode, $author$project$DataTypes$Overall),
							true)
						])
					])));
	});
var $author$project$DataChooser$qButtonText = function (_v0) {
	var axesConfig = _v0.aN;
	var questions = _v0.bd;
	var _v1 = _Utils_Tuple2(axesConfig, questions);
	if (_v1.a === 1) {
		if (!_v1.b.b) {
			var _v2 = _v1.a;
			return _Utils_Tuple2('Pick a survey question', 'Hide question chooser');
		} else {
			var _v3 = _v1.a;
			return _Utils_Tuple2('Add or remove survey questions', 'Hide question chooser');
		}
	} else {
		if (!_v1.b.b) {
			var _v4 = _v1.a;
			return _Utils_Tuple2('Pick a survey question', 'Pick a survey question');
		} else {
			var _v5 = _v1.a;
			return _Utils_Tuple2('Change survey question', 'Hide question chooser');
		}
	}
};
var $author$project$DataTypes$ChangeQuestion = function (a) {
	return {$: 12, a: a};
};
var $author$project$DataTypes$ClearAllQuestions = {$: 15};
var $author$project$DataTypes$SelectAllQuestions = {$: 16};
var $author$project$DataTypes$ToggleQuestion = function (a) {
	return {$: 11, a: a};
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $author$project$DataChooser$clearAll = function (msg) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$svg$Svg$Events$onClick(msg),
					$elm$html$Html$Attributes$class('defaultbutton')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Clear all')
				]))
		]);
};
var $author$project$DataChooser$chooserCheckBox = F6(
	function (enabled, checkBoxType, alreadyChosen, codes, msgFunc, thisChoice) {
		var ticked = A2($elm$core$List$member, thisChoice, alreadyChosen);
		var thisEnabled = A2($elm$core$List$member, thisChoice, enabled);
		var label = function () {
			var _v0 = A2($elm$core$Dict$get, thisChoice, codes);
			if (_v0.$ === 1) {
				return $elm$core$String$fromInt(thisChoice);
			} else {
				var str = _v0.a;
				return str;
			}
		}();
		return A5(
			$author$project$DataChooser$checkBox,
			checkBoxType,
			msgFunc(thisChoice),
			label,
			ticked,
			thisEnabled);
	});
var $author$project$DataChooser$dataChooser = F5(
	function (enabled, checkBoxType, alreadyChosen, codes, msgFunc) {
		return A2(
			$elm$core$List$map,
			A5($author$project$DataChooser$chooserCheckBox, enabled, checkBoxType, alreadyChosen, codes, msgFunc),
			$elm$core$Dict$keys(codes));
	});
var $author$project$DataChooser$selectAll = function (msg) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$svg$Svg$Events$onClick(msg),
					$elm$html$Html$Attributes$class('defaultbutton')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Select all')
				]))
		]);
};
var $author$project$DataChooser$questionChooserButtons = F2(
	function (axConf, questions) {
		var contents = function () {
			if (axConf === 1) {
				return _List_fromArray(
					[
						$author$project$DataChooser$clearAll($author$project$DataTypes$ClearAllQuestions),
						$author$project$DataChooser$selectAll($author$project$DataTypes$SelectAllQuestions),
						A5(
						$author$project$DataChooser$dataChooser,
						$elm$core$Dict$keys($author$project$DataTypes$questionCodes),
						1,
						questions,
						$author$project$DataTypes$questionCodes,
						$author$project$DataTypes$ToggleQuestion)
					]);
			} else {
				return _List_fromArray(
					[
						A5(
						$author$project$DataChooser$dataChooser,
						$elm$core$Dict$keys($author$project$DataTypes$questionCodes),
						0,
						questions,
						$author$project$DataTypes$questionCodes,
						$author$project$DataTypes$ChangeQuestion)
					]);
			}
		}();
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('chooserSet')
				]),
			$elm$core$List$concat(contents));
	});
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$DataChooser$showChooserButton = F3(
	function (showing, msg, _v0) {
		var showMsg = _v0.a;
		var hideMsg = _v0.b;
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$svg$Svg$Events$onClick(msg),
						$elm$html$Html$Attributes$class('defaultbutton')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('buttonText')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								showing ? hideMsg : showMsg)
							]))
					]))
			]);
	});
var $author$project$DataChooser$subButtonTxt = function (subject) {
	if (subject.$ === 1) {
		return 'Pick a subject area';
	} else {
		return 'Change subject area';
	}
};
var $author$project$DataTypes$ChangeSubject = function (a) {
	return {$: 9, a: a};
};
var $author$project$DataTypes$SubjectChooserSearchText = function (a) {
	return {$: 5, a: a};
};
var $author$project$DataChooser$filterUnis = function (results) {
	return $elm$core$Dict$filter(
		F2(
			function (_v0, v) {
				return A2($elm$core$List$member, v, results);
			}));
};
var $author$project$DataChooser$inRedMsgSubject = F2(
	function (unis, chartMode) {
		var _v0 = _Utils_Tuple2(chartMode, unis);
		if (!_v0.a.$) {
			var _v1 = _v0.a;
			return _List_Nil;
		} else {
			if (!_v0.b.b) {
				return _List_Nil;
			} else {
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('redMsg')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('No data is available for subjects in red for the ' + 'current university.')
							]))
					]);
			}
		}
	});
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$DataChooser$searchBox = F3(
	function (searchText, defaultText, msg) {
		return A2(
			$elm$html$Html$input,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$placeholder(defaultText),
					$elm$html$Html$Attributes$value(searchText),
					$elm$html$Html$Events$onInput(msg),
					$elm$html$Html$Attributes$class('searchBox'),
					$elm$html$Html$Attributes$class('defaultbutton')
				]),
			_List_Nil);
	});
var $author$project$DataChooser$subjectChooserButtons = F6(
	function (axConf, chartMode, unis, searchText, searchResults, subject) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('chooserSet')
				]),
			$elm$core$List$concat(
				_List_fromArray(
					[
						_List_fromArray(
						[
							A3($author$project$DataChooser$searchBox, searchText, 'Search for a subject', $author$project$DataTypes$SubjectChooserSearchText)
						]),
						function () {
						if (!axConf) {
							return _List_Nil;
						} else {
							return A2($author$project$DataChooser$inRedMsgSubject, unis, chartMode);
						}
					}(),
						A5(
						$author$project$DataChooser$dataChooser,
						function () {
							if (unis.b && (!unis.b.b)) {
								var uni = unis.a;
								var _v2 = A2($elm$core$Dict$get, uni, $author$project$Data$subjectsOffered);
								if (!_v2.$) {
									var subjects = _v2.a;
									return subjects;
								} else {
									return $elm$core$Dict$keys($author$project$Data$subjectCodes);
								}
							} else {
								return $elm$core$Dict$keys($author$project$Data$subjectCodes);
							}
						}(),
						0,
						function () {
							if (subject.$ === 1) {
								return _List_Nil;
							} else {
								var a = subject.a;
								return _List_fromArray(
									[a]);
							}
						}(),
						function () {
							if (!searchResults.b) {
								return $author$project$Data$subjectCodes;
							} else {
								return A2($author$project$DataChooser$filterUnis, searchResults, $author$project$Data$subjectCodes);
							}
						}(),
						$author$project$DataTypes$ChangeSubject)
					])));
	});
var $author$project$DataChooser$uniButtonText = function (_v0) {
	var axesConfig = _v0.aN;
	var unis = _v0.bv;
	var _v1 = _Utils_Tuple2(axesConfig, unis);
	if (!_v1.a) {
		if (!_v1.b.b) {
			var _v2 = _v1.a;
			return _Utils_Tuple2('Choose some universities', 'Hide university chooser');
		} else {
			var _v3 = _v1.a;
			return _Utils_Tuple2('Add or remove universities', 'Hide university chooser');
		}
	} else {
		if (!_v1.b.b) {
			var _v4 = _v1.a;
			return _Utils_Tuple2('Pick a university', 'Hide university chooser');
		} else {
			var _v5 = _v1.a;
			return _Utils_Tuple2('Change university', 'Hide university chooser');
		}
	}
};
var $author$project$DataTypes$UniChooserSearchText = function (a) {
	return {$: 4, a: a};
};
var $author$project$DataChooser$availableUnis = F2(
	function (chartMode, axConf) {
		if (chartMode.$ === 1) {
			if (!chartMode.a.$) {
				var subject = chartMode.a.a;
				return A2($elm$core$Dict$get, subject, $author$project$Data$unisOffering);
			} else {
				var _v1 = chartMode.a;
				return $elm$core$Maybe$Just(
					$elm$core$Dict$keys($author$project$Data$uniCodes));
			}
		} else {
			return $elm$core$Maybe$Just(
				$elm$core$Dict$keys($author$project$Data$overallUniCodes));
		}
	});
var $author$project$DataTypes$ChangeUni = function (a) {
	return {$: 13, a: a};
};
var $author$project$DataTypes$ToggleUni = function (a) {
	return {$: 10, a: a};
};
var $author$project$DataChooser$checkBoxMsg = function (axConf) {
	if (!axConf) {
		return $author$project$DataTypes$ToggleUni;
	} else {
		return $author$project$DataTypes$ChangeUni;
	}
};
var $author$project$DataTypes$ClearAllUnis = {$: 14};
var $author$project$DataTypes$SelectAllUnis = {$: 17};
var $author$project$DataChooser$clearAndSelect = function (axConf) {
	if (!axConf) {
		return $elm$core$List$concat(
			_List_fromArray(
				[
					$author$project$DataChooser$clearAll($author$project$DataTypes$ClearAllUnis),
					$author$project$DataChooser$selectAll($author$project$DataTypes$SelectAllUnis)
				]));
	} else {
		return _List_Nil;
	}
};
var $author$project$DataChooser$foundBySearch = F2(
	function (chartMode, searchResults) {
		var codes = function () {
			if (!chartMode.$) {
				return $author$project$Data$overallUniCodes;
			} else {
				return $author$project$Data$uniCodes;
			}
		}();
		if (!searchResults.b) {
			return codes;
		} else {
			return A2($author$project$DataChooser$filterUnis, searchResults, codes);
		}
	});
var $author$project$DataChooser$inRedMsgUni = function (chartMode) {
	if (!chartMode.$) {
		return _List_Nil;
	} else {
		if (!chartMode.a.$) {
			return _List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('redMsg')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('No data is available for universities in red ' + 'for the current subject area.')
						]))
				]);
		} else {
			var _v1 = chartMode.a;
			return _List_Nil;
		}
	}
};
var $author$project$DataChooser$radioOrTick = function (axConf) {
	if (!axConf) {
		return 1;
	} else {
		return 0;
	}
};
var $author$project$DataChooser$uniChooserButtons = F5(
	function (chartMode, axConf, searchText, searchResults, unis) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('chooserSet')
				]),
			$elm$core$List$concat(
				function () {
					var _v0 = A2($author$project$DataChooser$availableUnis, chartMode, axConf);
					if (!_v0.$) {
						var available = _v0.a;
						return _List_fromArray(
							[
								_List_fromArray(
								[
									A3($author$project$DataChooser$searchBox, searchText, 'Search for a university', $author$project$DataTypes$UniChooserSearchText)
								]),
								$author$project$DataChooser$clearAndSelect(axConf),
								$author$project$DataChooser$inRedMsgUni(chartMode),
								A5(
								$author$project$DataChooser$dataChooser,
								available,
								$author$project$DataChooser$radioOrTick(axConf),
								unis,
								A2($author$project$DataChooser$foundBySearch, chartMode, searchResults),
								$author$project$DataChooser$checkBoxMsg(axConf))
							]);
					} else {
						return _List_Nil;
					}
				}()));
	});
var $author$project$DataChooser$dataChooserButtons_ = F2(
	function (_v0, model) {
		var uni = _v0.aE;
		var question = _v0.D;
		var subject = _v0.bl;
		var uniSearchText = _v0.F;
		var subjectSearchText = _v0.E;
		return $elm$core$List$concat(
			_List_fromArray(
				[
					_List_fromArray(
					[
						A2($author$project$DataChooser$axesConfigMode, model.aN, model.X)
					]),
					A3(
					$author$project$DataChooser$showChooserButton,
					uni,
					$author$project$DataTypes$ChooseAUni,
					$author$project$DataChooser$uniButtonText(model)),
					uni ? _List_fromArray(
					[
						A5($author$project$DataChooser$uniChooserButtons, model.X, model.aN, uniSearchText, model.bu, model.bv)
					]) : _List_Nil,
					A3(
					$author$project$DataChooser$showChooserButton,
					question,
					$author$project$DataTypes$ChooseAQuestion,
					$author$project$DataChooser$qButtonText(model)),
					question ? _List_fromArray(
					[
						A2($author$project$DataChooser$questionChooserButtons, model.aN, model.bd)
					]) : _List_Nil,
					function () {
					var _v1 = model.X;
					if (_v1.$ === 1) {
						var maybeSubject = _v1.a;
						return A3(
							$author$project$DataChooser$showChooserButton,
							subject,
							$author$project$DataTypes$ChooseASubject,
							function () {
								var txt = $author$project$DataChooser$subButtonTxt(maybeSubject);
								return _Utils_Tuple2(txt, txt);
							}());
					} else {
						return _List_Nil;
					}
				}(),
					function () {
					var _v2 = model.X;
					if (_v2.$ === 1) {
						var maybeSubject = _v2.a;
						return subject ? _List_fromArray(
							[
								A6($author$project$DataChooser$subjectChooserButtons, model.aN, model.X, model.bv, subjectSearchText, model.bn, maybeSubject)
							]) : _List_Nil;
					} else {
						return _List_Nil;
					}
				}()
				]));
	});
var $author$project$DataChooser$defaultConf = {D: false, bl: false, E: '', aE: false, F: ''};
var $author$project$DataChooser$makeChooserConfig = function (chooserMode) {
	switch (chooserMode.$) {
		case 2:
			var searchText = chooserMode.a;
			return _Utils_update(
				$author$project$DataChooser$defaultConf,
				{aE: true, F: searchText});
		case 0:
			return _Utils_update(
				$author$project$DataChooser$defaultConf,
				{D: true});
		case 1:
			var searchText = chooserMode.a;
			return _Utils_update(
				$author$project$DataChooser$defaultConf,
				{bl: true, E: searchText});
		default:
			return $author$project$DataChooser$defaultConf;
	}
};
var $author$project$DataChooser$dataChooserButtons = function (model) {
	return A2(
		$author$project$DataChooser$dataChooserButtons_,
		$author$project$DataChooser$makeChooserConfig(model.aR),
		model);
};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $author$project$View$explainConfidenceIntervals = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('explainP')
		]),
	_List_fromArray(
		[
			$elm$html$Html$text('The dot gives the percentage of students who answered "Mostly agree" or "Definitely agree".  The line gives the '),
			A2(
			$elm$html$Html$a,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$href('https://www.officeforstudents.org.uk/advice-and-guidance/student-information-and-data/national-student-survey-nss/questions-about-the-nss-data/')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('95% confidence interval')
				])),
			$elm$html$Html$text('.')
		]));
var $elm$html$Html$i = _VirtualDom_node('i');
var $author$project$Chart$bySubjectQVsA = F2(
	function (subject, uni) {
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text('Percentage of students studying ')
					])),
				A2(
				$elm$html$Html$i,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text(subject)
					])),
				A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text(' at ')
					])),
				A2(
				$elm$html$Html$i,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text(uni)
					])),
				A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text(' who agreed with each of the survey questions.')
					]))
			]);
	});
var $author$project$Chart$overallQVsA = function (uni) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$span,
			_List_Nil,
			_List_fromArray(
				[
					$elm$svg$Svg$text('Percentage of students at ')
				])),
			A2(
			$elm$html$Html$i,
			_List_Nil,
			_List_fromArray(
				[
					$elm$svg$Svg$text(uni)
				])),
			A2(
			$elm$html$Html$span,
			_List_Nil,
			_List_fromArray(
				[
					$elm$svg$Svg$text(' who agreed with each of the survey questions.')
				]))
		]);
};
var $author$project$Chart$percentWhoAgreed = function (question) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$span,
			_List_Nil,
			_List_fromArray(
				[
					$elm$svg$Svg$text('Percentage of students who agreed with ')
				])),
			A2(
			$elm$html$Html$i,
			_List_Nil,
			_List_fromArray(
				[
					$elm$svg$Svg$text(question)
				]))
		]);
};
var $author$project$Chart$whoWereStudying = F2(
	function (question, subject) {
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text('Percentage of students studying ')
					])),
				A2(
				$elm$html$Html$i,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text(subject)
					])),
				A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text(' who agreed with ')
					])),
				A2(
				$elm$html$Html$i,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text(question)
					]))
			]);
	});
var $author$project$Chart$makeCaption = function (model) {
	var _v0 = _Utils_Tuple2(
		_Utils_Tuple2(model.aN, model.X),
		_Utils_Tuple2(model.bd, model.bv));
	_v0$7:
	while (true) {
		if (!_v0.a.a) {
			if (!_v0.a.b.$) {
				if (_v0.b.a.b && (!_v0.b.a.b.b)) {
					var _v1 = _v0.a;
					var _v2 = _v1.a;
					var _v3 = _v1.b;
					var _v4 = _v0.b;
					var _v5 = _v4.a;
					var questionNum = _v5.a;
					var _v6 = A2($elm$core$Dict$get, questionNum, $author$project$DataTypes$questionCodes);
					if (_v6.$ === 1) {
						return _List_Nil;
					} else {
						var question = _v6.a;
						return $author$project$Chart$percentWhoAgreed(question);
					}
				} else {
					var _v7 = _v0.a;
					var _v8 = _v7.a;
					var _v9 = _v7.b;
					var _v10 = _v0.b;
					return _List_Nil;
				}
			} else {
				if (_v0.a.b.a.$ === 1) {
					var _v11 = _v0.a;
					var _v12 = _v11.a;
					var _v13 = _v11.b.a;
					var _v14 = _v0.b;
					return _List_Nil;
				} else {
					if (_v0.b.a.b && (!_v0.b.a.b.b)) {
						var _v15 = _v0.a;
						var _v16 = _v15.a;
						var subjectNum = _v15.b.a.a;
						var _v17 = _v0.b;
						var _v18 = _v17.a;
						var questionNum = _v18.a;
						var _v19 = _Utils_Tuple2(
							A2($elm$core$Dict$get, subjectNum, $author$project$Data$subjectCodes),
							A2($elm$core$Dict$get, questionNum, $author$project$DataTypes$questionCodes));
						if ((!_v19.a.$) && (!_v19.b.$)) {
							var subject = _v19.a.a;
							var question = _v19.b.a;
							return A2($author$project$Chart$whoWereStudying, question, subject);
						} else {
							return _List_Nil;
						}
					} else {
						var _v20 = _v0.a;
						var _v21 = _v20.a;
						var _v22 = _v0.b;
						return _List_Nil;
					}
				}
			}
		} else {
			if (!_v0.a.b.$) {
				if (_v0.b.b.b && (!_v0.b.b.b.b)) {
					var _v23 = _v0.a;
					var _v24 = _v23.a;
					var _v25 = _v23.b;
					var _v26 = _v0.b;
					var _v27 = _v26.b;
					var uniNum = _v27.a;
					var _v28 = A2($elm$core$Dict$get, uniNum, $author$project$Data$overallUniCodes);
					if (_v28.$ === 1) {
						return _List_Nil;
					} else {
						var uni = _v28.a;
						return $author$project$Chart$overallQVsA(uni);
					}
				} else {
					break _v0$7;
				}
			} else {
				if (((!_v0.a.b.a.$) && _v0.b.b.b) && (!_v0.b.b.b.b)) {
					var _v29 = _v0.a;
					var _v30 = _v29.a;
					var subjectNum = _v29.b.a.a;
					var _v31 = _v0.b;
					var _v32 = _v31.b;
					var uniNum = _v32.a;
					var _v33 = _Utils_Tuple2(
						A2($elm$core$Dict$get, subjectNum, $author$project$Data$subjectCodes),
						A2($elm$core$Dict$get, uniNum, $author$project$Data$uniCodes));
					if ((!_v33.a.$) && (!_v33.b.$)) {
						var subject = _v33.a.a;
						var uni = _v33.b.a;
						return A2($author$project$Chart$bySubjectQVsA, subject, uni);
					} else {
						return _List_Nil;
					}
				} else {
					break _v0$7;
				}
			}
		}
	}
	return _List_Nil;
};
var $author$project$View$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('topDiv')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('dataChooserDiv')
							]),
						$author$project$DataChooser$dataChooserButtons(model)),
						$author$project$View$explainConfidenceIntervals
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('chartCaption')
					]),
				$author$project$Chart$makeCaption(model)),
				$author$project$View$chartOrMsg(model)
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{
		a2: $author$project$Main$init,
		bp: function (_v0) {
			return $elm$core$Platform$Sub$none;
		},
		bw: $author$project$Update$update,
		bx: $author$project$View$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));