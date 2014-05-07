/** @constructor
*/
var RingOscillator = function(){
/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function Fay$$_(thunkish,nocache){
  while (thunkish instanceof Fay$$$) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function Fay$$__(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof Fay$$$? Fay$$_(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function Fay$$$(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
Fay$$$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};


function Fay$$seq(x) {
  return function(y) {
    Fay$$_(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  Fay$$_(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new Fay$$$(function(){
      var monad = Fay$$_(m,true);
      return Fay$$_(f)(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
  return new Fay$$$(function(){
    var monad = Fay$$_(m,true);
    return Fay$$_(f)(monad.value);
  });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new Fay$$$(function(){
      Fay$$_(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  if(base == "action") {
    // A nullary monadic action. Should become a nullary JS function.
    // Fay () -> function(){ return ... }
    jsObj = function(){
      return Fay$$fayToJs(args[0],Fay$$_(fayObj,true).value);
    };

  }
  else if(base == "function") {
    // A proper function.
    jsObj = function(){
      var fayFunc = fayObj;
      var return_type = args[args.length-1];
      var len = args.length;
      // If some arguments.
      if (len > 1) {
        // Apply to all the arguments.
        fayFunc = Fay$$_(fayFunc,true);
        // TODO: Perhaps we should throw an error when JS
        // passes more arguments than Haskell accepts.
        for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
          // Unserialize the JS values to Fay for the Fay callback.
          fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
        }
        // Finally, serialize the Fay return value back to JS.
        var return_base = return_type[0];
        var return_args = return_type[1];
        // If it's a monadic return value, get the value instead.
        if(return_base == "action") {
          return Fay$$fayToJs(return_args[0],fayFunc.value);
        }
        // Otherwise just serialize the value direct.
        else {
          return Fay$$fayToJs(return_type,fayFunc);
        }
      } else {
        throw new Error("Nullary function?");
      }
    };

  }
  else if(base == "string") {
    jsObj = Fay$$fayToJs_string(fayObj);
  }
  else if(base == "list") {
    // Serialize Fay list to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[0],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    jsObj = arr;

  }
  else if(base == "tuple") {
    // Serialize Fay tuple to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    var i = 0;
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[i++],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    jsObj = arr;

  }
  else if(base == "defined") {
    fayObj = Fay$$_(fayObj);
    if (fayObj instanceof $_Language$Fay$FFI$Undefined) {
      jsObj = undefined;
    } else {
      jsObj = Fay$$fayToJsUserDefined(args[0],fayObj["slot1"]);
    }

  }
  else if(base == "nullable") {
    fayObj = Fay$$_(fayObj);
    if (fayObj instanceof $_Language$Fay$FFI$Null) {
      jsObj = null;
    } else {
      jsObj = Fay$$fayToJsUserDefined(args[0],fayObj["slot1"]);
    }

  }
  else if(base == "double" || base == "int" || base == "bool") {
    // Bools are unboxed.
    jsObj = Fay$$_(fayObj);

  }
  else if(base == "ptr" || base == "unknown")
    return fayObj;
  else if(base == "automatic" || base == "user") {
    if(fayObj instanceof Fay$$$)
      fayObj = Fay$$_(fayObj);
    jsObj = Fay$$fayToJsUserDefined(type,fayObj);

  }
  else
    throw new Error("Unhandled Fay->JS translation type: " + base);
  return jsObj;
}

// Specialized serializer for string.
function Fay$$fayToJs_string(fayObj){
  // Serialize Fay string to JavaScript string.
  var str = "";
  fayObj = Fay$$_(fayObj);
  while(fayObj instanceof Fay$$Cons) {
    str += fayObj.car;
    fayObj = Fay$$_(fayObj.cdr);
  }
  return str;
};
function Fay$$jsToFay_string(x){
  return Fay$$list(x)
};

// Special num/bool serializers.
function Fay$$jsToFay_int(x){return x;}
function Fay$$jsToFay_double(x){return x;}
function Fay$$jsToFay_bool(x){return x;}

function Fay$$fayToJs_int(x){return Fay$$_(x);}
function Fay$$fayToJs_double(x){return Fay$$_(x);}
function Fay$$fayToJs_bool(x){return Fay$$_(x);}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  if(base == "action") {
    // Unserialize a "monadic" JavaScript return value into a monadic value.
    fayObj = new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));

  }
  else if(base == "string") {
    // Unserialize a JS string into Fay list (String).
    fayObj = Fay$$list(jsObj);
  }
  else if(base == "list") {
    // Unserialize a JS array into a Fay list ([a]).
    var serializedList = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
    }
    // Pop it all in a Fay list.
    fayObj = Fay$$list(serializedList);

  }
  else if(base == "tuple") {
    // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
    var serializedTuple = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
    }
    // Pop it all in a Fay list.
    fayObj = Fay$$list(serializedTuple);

  }
  else if(base == "defined") {
    if (jsObj === undefined) {
      fayObj = new $_Language$Fay$FFI$Undefined();
    } else {
      fayObj = new $_Language$Fay$FFI$Defined(Fay$$jsToFay(args[0],jsObj));
    }

  }
  else if(base == "nullable") {
    if (jsObj === null) {
      fayObj = new $_Language$Fay$FFI$Null();
    } else {
      fayObj = new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(args[0],jsObj));
    }

  }
  else if(base == "int") {
    // Int are unboxed, so there's no forcing to do.
    // But we can do validation that the int has no decimal places.
    // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
    fayObj = Math.round(jsObj);
    if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";

  }
  else if (base == "double" ||
           base == "bool" ||
           base ==  "ptr" ||
           base ==  "unknown") {
    return jsObj;
  }
  else if(base == "automatic" || base == "user") {
    if (jsObj && jsObj['instance']) {
      fayObj = Fay$$jsToFayUserDefined(type,jsObj);
    }
    else
      fayObj = jsObj;

  }
  else { throw new Error("Unhandled JS->Fay translation type: " + base); }
  return fayObj;
}

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
// `list' is already forced by the time it's passed to this function.
// `list' cannot be null and `index' cannot be out of bounds.
function Fay$$index(index,list){
  for(var i = 0; i < index; i++) {
    list = Fay$$_(list.cdr);
  }
  return list.car;
}

// List length.
// `list' is already forced by the time it's passed to this function.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = Fay$$_(list.cdr);
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) * Fay$$_(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) * Fay$$_(y);
  });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) + Fay$$_(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) + Fay$$_(y);
  });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) - Fay$$_(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) - Fay$$_(y);
  });

}

// Built-in /.
function Fay$$divi(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) / Fay$$_(y);
    });
  };
}

// Built-in /.
function Fay$$divi$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) / Fay$$_(y);
  });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = Fay$$_(lit1);
  lit2 = Fay$$_(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = Fay$$_(lit1.cdr), lit2 = Fay$$_(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.constructor === lit2.constructor) {
    for(var x in lit1) {
      if(!(lit1.hasOwnProperty(x) && lit2.hasOwnProperty(x) &&
           Fay$$equal(lit1[x],lit2[x])))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$equal(x,y);
  });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new Fay$$$(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return !(Fay$$equal(x,y));
  });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) > Fay$$_(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) > Fay$$_(y);
  });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) < Fay$$_(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) < Fay$$_(y);
  });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) >= Fay$$_(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) >= Fay$$_(y);
  });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) <= Fay$$_(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) <= Fay$$_(y);
  });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) && Fay$$_(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) && Fay$$_(y);
  });
  ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) || Fay$$_(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) || Fay$$_(y);
  });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */

var Language$Fay$FFI$Nullable = function(slot1){return new Fay$$$(function(){return new $_Language$Fay$FFI$Nullable(slot1);});};var Language$Fay$FFI$Null = new Fay$$$(function(){return new $_Language$Fay$FFI$Null();});var Language$Fay$FFI$Defined = function(slot1){return new Fay$$$(function(){return new $_Language$Fay$FFI$Defined(slot1);});};var Language$Fay$FFI$Undefined = new Fay$$$(function(){return new $_Language$Fay$FFI$Undefined();});var Language$Fay$FFI$Nullable = function(slot1){return new Fay$$$(function(){return new $_Language$Fay$FFI$Nullable(slot1);});};var Language$Fay$FFI$Null = new Fay$$$(function(){return new $_Language$Fay$FFI$Null();});var Language$Fay$FFI$Defined = function(slot1){return new Fay$$$(function(){return new $_Language$Fay$FFI$Defined(slot1);});};var Language$Fay$FFI$Undefined = new Fay$$$(function(){return new $_Language$Fay$FFI$Undefined();});var Prelude$Just = function(slot1){return new Fay$$$(function(){return new $_Prelude$Just(slot1);});};var Prelude$Nothing = new Fay$$$(function(){return new $_Prelude$Nothing();});var Prelude$Left = function(slot1){return new Fay$$$(function(){return new $_Prelude$Left(slot1);});};var Prelude$Right = function(slot1){return new Fay$$$(function(){return new $_Prelude$Right(slot1);});};var Prelude$maybe = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof $_Prelude$Nothing) {var m = $p1;return m;}if (Fay$$_($p3) instanceof $_Prelude$Just) {var x = Fay$$_($p3).slot1;var f = $p2;return Fay$$_(f)(x);}throw ["unhandled case in maybe",[$p1,$p2,$p3]];});};};};var Prelude$Ratio = function(slot1){return function(slot2){return new Fay$$$(function(){return new $_Prelude$Ratio(slot1,slot2);});};};var Prelude$$62$$62$$61$ = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$bind(Fay$$fayToJs(["action",[["unknown"]]],$p1))(Fay$$fayToJs(["function",[["unknown"],["action",[["unknown"]]]]],$p2))));});};};var Prelude$$62$$62$ = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$then(Fay$$fayToJs(["action",[["unknown"]]],$p1))(Fay$$fayToJs(["action",[["unknown"]]],$p2))));});};};var Prelude$$_return = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$return(Fay$$fayToJs(["unknown"],$p1))));});};var Prelude$when = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var p = $p1;return Fay$$_(p) ? Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Fay$$$_return)(Fay$$unit)) : Fay$$_(Fay$$$_return)(Fay$$unit);});};};var Prelude$forM_ = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude$forM_)(xs))(m));}if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in forM_",[$p1,$p2]];});};};var Prelude$mapM_ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude$mapM_)(m))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in mapM_",[$p1,$p2]];});};};var Prelude$$61$$60$$60$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(x))(f);});};};var Prelude$sequence = function($p1){return new Fay$$$(function(){var ms = $p1;return (function(){var k = function($p1){return function($p2){return new Fay$$$(function(){var m$39$ = $p2;var m = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m))(function($p1){var x = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m$39$))(function($p1){var xs = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(x))(xs));});});});};};return Fay$$_(Fay$$_(Fay$$_(Prelude$foldr)(k))(Fay$$_(Fay$$$_return)(null)))(ms);})();});};var Prelude$sequence_ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var m = $tmp1.car;var ms = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Prelude$sequence_)(ms));}throw ["unhandled case in sequence_",[$p1]];});};var Prelude$GT = new Fay$$$(function(){return new $_Prelude$GT();});var Prelude$LT = new Fay$$$(function(){return new $_Prelude$LT();});var Prelude$EQ = new Fay$$$(function(){return new $_Prelude$EQ();});var Prelude$compare = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(y)) ? Prelude$GT : Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(y)) ? Prelude$LT : Prelude$EQ;});};};var Prelude$succ = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$add)(x))(1);});};var Prelude$pred = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$sub)(x))(1);});};var Prelude$enumFrom = function($p1){return new Fay$$$(function(){var i = $p1;return Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Prelude$enumFrom)(Fay$$_(Fay$$_(Fay$$add)(i))(1)));});};var Prelude$enumFromTo = function($p1){return function($p2){return new Fay$$$(function(){var n = $p2;var i = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(i))(n)) ? null : Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Fay$$_(Prelude$enumFromTo)(Fay$$_(Fay$$_(Fay$$add)(i))(1)))(n));});};};var Prelude$enumFromBy = function($p1){return function($p2){return new Fay$$$(function(){var by = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$cons)(fr))(Fay$$_(Fay$$_(Prelude$enumFromBy)(Fay$$_(Fay$$_(Fay$$add)(fr))(by)))(by));});};};var Prelude$enumFromThen = function($p1){return function($p2){return new Fay$$$(function(){var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Prelude$enumFromBy)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr));});};};var Prelude$enumFromByTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var by = $p2;var fr = $p1;return (function(){var neg = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(neg)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};var pos = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(pos)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(by))(0)) ? Fay$$_(neg)(fr) : Fay$$_(pos)(fr);})();});};};};var Prelude$enumFromThenTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$enumFromByTo)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr)))(to);});};};};var Prelude$fromIntegral = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs_int($p1));});};var Prelude$fromInteger = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs_int($p1));});};var Prelude$not = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(p) ? false : true;});};var Prelude$otherwise = true;var Prelude$show = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_string(JSON.stringify(Fay$$fayToJs(["automatic"],$p1)));});};var Prelude$error = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());});};var Prelude$$_undefined = new Fay$$$(function(){return Fay$$_(Prelude$error)(Fay$$list("Prelude.undefined"));});var Prelude$either = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof $_Prelude$Left) {var a = Fay$$_($p3).slot1;var f = $p1;return Fay$$_(f)(a);}if (Fay$$_($p3) instanceof $_Prelude$Right) {var b = Fay$$_($p3).slot1;var g = $p2;return Fay$$_(g)(b);}throw ["unhandled case in either",[$p1,$p2,$p3]];});};};};var Prelude$until = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var f = $p2;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? x : Fay$$_(Fay$$_(Fay$$_(Prelude$until)(p))(f))(Fay$$_(f)(x));});};};};var Prelude$$36$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$seq)(x))(Fay$$_(f)(x));});};};var Prelude$$_const = function($p1){return function($p2){return new Fay$$$(function(){var a = $p1;return a;});};};var Prelude$id = function($p1){return new Fay$$$(function(){var x = $p1;return x;});};var Prelude$$46$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var g = $p2;var f = $p1;return Fay$$_(f)(Fay$$_(g)(x));});};};};var Prelude$$36$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(f)(x);});};};var Prelude$flip = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(y))(x);});};};};var Prelude$curry = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(f)(Fay$$list([x,y]));});};};};var Prelude$uncurry = function($p1){return function($p2){return new Fay$$$(function(){var p = $p2;var f = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var x = Fay$$index(0,Fay$$_($tmp1));var y = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(f)(x))(y);}return (function(){ throw (["unhandled case",$tmp1]); })();})(p);});};};var Prelude$snd = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(1,Fay$$_($p1));return x;}throw ["unhandled case in snd",[$p1]];});};var Prelude$fst = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(0,Fay$$_($p1));return x;}throw ["unhandled case in fst",[$p1]];});};var Prelude$div = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$quot)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$quot)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude$quot)(x))(y);});};};var Prelude$mod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$rem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$rem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude$rem)(x))(y);});};};var Prelude$divMod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$quotRem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(1)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$quotRem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y));}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude$quotRem)(x))(y);});};};var Prelude$min = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.min(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Prelude$max = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.max(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Prelude$recip = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(1))(x);});};var Prelude$negate = function($p1){return new Fay$$$(function(){var x = $p1;return (-(Fay$$_(x)));});};var Prelude$abs = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude$negate)(x) : x;});};var Prelude$signum = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(0)) ? 1 : Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(x))(0)) ? 0 : (-(1));});};var Prelude$pi = new Fay$$$(function(){return Fay$$jsToFay_double(Math.PI);});var Prelude$exp = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));});};var Prelude$sqrt = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));});};var Prelude$log = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));});};var Prelude$$42$$42$ = new Fay$$$(function(){return Prelude$unsafePow;});var Prelude$$94$$94$ = new Fay$$$(function(){return Prelude$unsafePow;});var Prelude$unsafePow = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.pow(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Prelude$$94$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0))) {return Fay$$_(Prelude$error)(Fay$$list("(^): negative exponent"));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(b))(0))) {return 1;} else {if (Fay$$_(Fay$$_(Prelude$even)(b))) {return (function(){var x = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$94$)(a))(Fay$$_(Fay$$_(Prelude$quot)(b))(2));});return Fay$$_(Fay$$_(Fay$$mult)(x))(x);})();}}}var b = $p2;var a = $p1;return Fay$$_(Fay$$_(Fay$$mult)(a))(Fay$$_(Fay$$_(Prelude$$94$)(a))(Fay$$_(Fay$$_(Fay$$sub)(b))(1)));});};};var Prelude$logBase = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var b = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude$log)(x)))(Fay$$_(Prelude$log)(b));});};};var Prelude$sin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sin(Fay$$fayToJs_double($p1)));});};var Prelude$tan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.tan(Fay$$fayToJs_double($p1)));});};var Prelude$cos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.cos(Fay$$fayToJs_double($p1)));});};var Prelude$asin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.asin(Fay$$fayToJs_double($p1)));});};var Prelude$atan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.atan(Fay$$fayToJs_double($p1)));});};var Prelude$acos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.acos(Fay$$fayToJs_double($p1)));});};var Prelude$sinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Prelude$exp)(x)))(Fay$$_(Prelude$exp)((-(Fay$$_(x)))))))(2);});};var Prelude$tanh = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){var a = new Fay$$$(function(){return Fay$$_(Prelude$exp)(x);});var b = new Fay$$$(function(){return Fay$$_(Prelude$exp)((-(Fay$$_(x))));});return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(a))(b)))(Fay$$_(Fay$$_(Fay$$add)(a))(b));})();});};var Prelude$cosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Prelude$exp)(x)))(Fay$$_(Prelude$exp)((-(Fay$$_(x)))))))(2);});};var Prelude$asinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude$log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude$sqrt)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$$42$$42$)(x))(2)))(1))));});};var Prelude$atanh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude$log)(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(1))(x)))(Fay$$_(Fay$$_(Fay$$sub)(1))(x)))))(2);});};var Prelude$acosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude$log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude$sqrt)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$$42$$42$)(x))(2)))(1))));});};var Prelude$properFraction = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){var a = new Fay$$$(function(){return Fay$$_(Prelude$truncate)(x);});return Fay$$list([a,Fay$$_(Fay$$_(Fay$$sub)(x))(Fay$$_(Prelude$fromIntegral)(a))]);})();});};var Prelude$truncate = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude$ceiling)(x) : Fay$$_(Prelude$floor)(x);});};var Prelude$round = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.round(Fay$$fayToJs_double($p1)));});};var Prelude$ceiling = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));});};var Prelude$floor = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));});};var Prelude$subtract = new Fay$$$(function(){return Fay$$_(Prelude$flip)(Fay$$sub);});var Prelude$even = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Fay$$_(Prelude$rem)(x))(2)))(0);});};var Prelude$odd = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude$not)(Fay$$_(Prelude$even)(x));});};var Prelude$gcd = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {var x = $p1;return x;}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(go)(y))(Fay$$_(Fay$$_(Prelude$rem)(x))(y));});};};return Fay$$_(Fay$$_(go)(Fay$$_(Prelude$abs)(a)))(Fay$$_(Prelude$abs)(b));})();});};};var Prelude$quot = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude$error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude$quot$39$)(x))(y);});};};var Prelude$quot$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(~~(Fay$$fayToJs_int($p1)/Fay$$fayToJs_int($p2)));});};};var Prelude$quotRem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$list([Fay$$_(Fay$$_(Prelude$quot)(x))(y),Fay$$_(Fay$$_(Prelude$rem)(x))(y)]);});};};var Prelude$rem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude$error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude$rem$39$)(x))(y);});};};var Prelude$rem$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(Fay$$fayToJs_int($p1) % Fay$$fayToJs_int($p2));});};};var Prelude$lcm = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {return 0;}if (Fay$$_($p1) === 0) {return 0;}var b = $p2;var a = $p1;return Fay$$_(Prelude$abs)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Prelude$quot)(a))(Fay$$_(Fay$$_(Prelude$gcd)(a))(b))))(b));});};};var Prelude$find = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Prelude$Just)(x) : Fay$$_(Fay$$_(Prelude$find)(p))(xs);}if (Fay$$_($p2) === null) {return Prelude$Nothing;}throw ["unhandled case in find",[$p1,$p2]];});};};var Prelude$filter = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$filter)(p))(xs)) : Fay$$_(Fay$$_(Prelude$filter)(p))(xs);}if (Fay$$_($p2) === null) {return null;}throw ["unhandled case in filter",[$p1,$p2]];});};};var Prelude$$_null = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}return false;});};var Prelude$map = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(f)(x)))(Fay$$_(Fay$$_(Prelude$map)(f))(xs));}throw ["unhandled case in map",[$p1,$p2]];});};};var Prelude$nub = function($p1){return new Fay$$$(function(){var ls = $p1;return Fay$$_(Fay$$_(Prelude$nub$39$)(ls))(null);});};var Prelude$nub$39$ = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var ls = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude$elem)(x))(ls)) ? Fay$$_(Fay$$_(Prelude$nub$39$)(xs))(ls) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$nub$39$)(xs))(Fay$$_(Fay$$_(Fay$$cons)(x))(ls)));}throw ["unhandled case in nub'",[$p1,$p2]];});};};var Prelude$elem = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var y = $tmp1.car;var ys = $tmp1.cdr;var x = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(x))(y)))(Fay$$_(Fay$$_(Prelude$elem)(x))(ys));}if (Fay$$_($p2) === null) {return false;}throw ["unhandled case in elem",[$p1,$p2]];});};};var Prelude$notElem = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var x = $p1;return Fay$$_(Prelude$not)(Fay$$_(Fay$$_(Prelude$elem)(x))(ys));});};};var Prelude$sort = new Fay$$$(function(){return Fay$$_(Prelude$sortBy)(Prelude$compare);});var Prelude$sortBy = function($p1){return new Fay$$$(function(){var cmp = $p1;return Fay$$_(Fay$$_(Prelude$foldr)(Fay$$_(Prelude$insertBy)(cmp)))(null);});};var Prelude$insertBy = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var x = $p2;return Fay$$list([x]);}var ys = $p3;var x = $p2;var cmp = $p1;return (function($tmp1){if (Fay$$_($tmp1) === null) {return Fay$$list([x]);}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var y = $tmp2.car;var ys$39$ = $tmp2.cdr;return (function($tmp2){if (Fay$$_($tmp2) instanceof $_Prelude$GT) {return Fay$$_(Fay$$_(Fay$$cons)(y))(Fay$$_(Fay$$_(Fay$$_(Prelude$insertBy)(cmp))(x))(ys$39$));}return Fay$$_(Fay$$_(Fay$$cons)(x))(ys);})(Fay$$_(Fay$$_(cmp)(x))(y));}return (function(){ throw (["unhandled case",$tmp1]); })();})(ys);});};};};var Prelude$conc = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$conc)(xs))(ys));}var ys = $p2;if (Fay$$_($p1) === null) {return ys;}throw ["unhandled case in conc",[$p1,$p2]];});};};var Prelude$concat = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$foldr)(Prelude$conc))(null);});var Prelude$concatMap = function($p1){return new Fay$$$(function(){var f = $p1;return Fay$$_(Fay$$_(Prelude$foldr)(Fay$$_(Fay$$_(Prelude$$46$)(Prelude$$43$$43$))(f)))(null);});};var Prelude$foldr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Fay$$_(Prelude$foldr)(f))(z))(xs));}throw ["unhandled case in foldr",[$p1,$p2,$p3]];});};};};var Prelude$foldr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return x;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Prelude$foldr1)(f))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Prelude$error)(Fay$$list("foldr1: empty list"));}throw ["unhandled case in foldr1",[$p1,$p2]];});};};var Prelude$foldl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}throw ["unhandled case in foldl",[$p1,$p2,$p3]];});};};};var Prelude$foldl1 = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(f))(x))(xs);}if (Fay$$_($p2) === null) {return Fay$$_(Prelude$error)(Fay$$list("foldl1: empty list"));}throw ["unhandled case in foldl1",[$p1,$p2]];});};};var Prelude$$43$$43$ = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude$conc)(x))(y);});};};var Prelude$$33$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("(!!): index too large"));}if (Fay$$_($p2) === 0) {var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}}var n = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Fay$$_(go)(t))(Fay$$_(Fay$$_(Fay$$sub)(n))(1));}throw ["unhandled case in go",[$p1,$p2]];});};};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0)) ? Fay$$_(Prelude$error)(Fay$$list("(!!): negative index")) : Fay$$_(Fay$$_(go)(a))(b);})();});};};var Prelude$head = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("head: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}throw ["unhandled case in head",[$p1]];});};var Prelude$tail = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("tail: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return t;}throw ["unhandled case in tail",[$p1]];});};var Prelude$init = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("init: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {var a = Fay$$index(0,Fay$$_($p1));return Fay$$list([a]);}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;var t = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(h))(Fay$$_(Prelude$init)(t));}throw ["unhandled case in init",[$p1]];});};var Prelude$last = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("last: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {var a = Fay$$index(0,Fay$$_($p1));return a;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Prelude$last)(t);}throw ["unhandled case in last",[$p1]];});};var Prelude$iterate = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$iterate)(f))(Fay$$_(f)(x)));});};};var Prelude$repeat = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Prelude$repeat)(x));});};var Prelude$replicate = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}var x = $p2;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("replicate: negative length")) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$replicate)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(x));});};};var Prelude$cycle = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("cycle: empty list"));}var xs = $p1;return (function(){var xs$39$ = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$43$$43$)(xs))(xs$39$);});return xs$39$;})();});};var Prelude$take = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("take: negative length")) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$take)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in take",[$p1,$p2]];});};};var Prelude$drop = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return xs;}if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("drop: negative length")) : Fay$$_(Fay$$_(Prelude$drop)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs);}throw ["unhandled case in drop",[$p1,$p2]];});};};var Prelude$splitAt = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return Fay$$list([null,xs]);}if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("splitAt: negative length")) : (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$splitAt)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in splitAt",[$p1,$p2]];});};};var Prelude$takeWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$takeWhile)(p))(xs)) : null;}throw ["unhandled case in takeWhile",[$p1,$p2]];});};};var Prelude$dropWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Prelude$dropWhile)(p))(xs) : Fay$$_(Fay$$_(Fay$$cons)(x))(xs);}throw ["unhandled case in dropWhile",[$p1,$p2]];});};};var Prelude$span = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$span)(p))(xs)) : Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]);}throw ["unhandled case in span",[$p1,$p2]];});};};var Prelude$$_break = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(Prelude$span)(Fay$$_(Fay$$_(Prelude$$46$)(Prelude$not))(p));});};var Prelude$zipWith = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(a))(b)))(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(f))(as))(bs));}}return null;});};};};var Prelude$zipWith3 = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var $tmp1 = Fay$$_($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(f)(a))(b))(c)))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith3)(f))(as))(bs))(cs));}}}return null;});};};};};var Prelude$zip = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b])))(Fay$$_(Fay$$_(Prelude$zip)(as))(bs));}}return null;});};};var Prelude$zip3 = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b,c])))(Fay$$_(Fay$$_(Fay$$_(Prelude$zip3)(as))(bs))(cs));}}}return null;});};};};var Prelude$unzip = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude$unzip)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null]);}throw ["unhandled case in unzip",[$p1]];});};var Prelude$unzip3 = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),3)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var z = Fay$$index(2,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),3)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));var zs = Fay$$index(2,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude$unzip3)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null,null]);}throw ["unhandled case in unzip3",[$p1]];});};var Prelude$lines = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function(){var isLineBreak = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(c))("\r")))(Fay$$_(Fay$$_(Fay$$eq)(c))("\n"));});};return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {return Fay$$list([a]);}var a = Fay$$index(0,Fay$$_($tmp1));var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));if ($tmp2 instanceof Fay$$Cons) {var cs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude$lines)(cs));}}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$$_break)(isLineBreak))(s));})();});};var Prelude$unlines = new Fay$$$(function(){return Fay$$_(Prelude$intercalate)(Fay$$list("\n"));});var Prelude$words = function($p1){return new Fay$$$(function(){var str = $p1;return (function(){var words$39$ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude$words)(b));}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$$_break)(isSpace))(s));});};var isSpace = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Prelude$elem)(c))(Fay$$list(" \t\r\n\u000c\u000b"));});};return Fay$$_(words$39$)(Fay$$_(Fay$$_(Prelude$dropWhile)(isSpace))(str));})();});};var Prelude$unwords = new Fay$$$(function(){return Fay$$_(Prelude$intercalate)(Fay$$list(" "));});var Prelude$and = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$and)(x))(Fay$$_(Prelude$and)(xs));}throw ["unhandled case in and",[$p1]];});};var Prelude$or = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return false;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$or)(x))(Fay$$_(Prelude$or)(xs));}throw ["unhandled case in or",[$p1]];});};var Prelude$any = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return false;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude$any)(p))(xs));}throw ["unhandled case in any",[$p1,$p2]];});};};var Prelude$all = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return true;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$and)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude$all)(p))(xs));}throw ["unhandled case in all",[$p1,$p2]];});};};var Prelude$intersperse = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$prependToAll)(sep))(xs));}throw ["unhandled case in intersperse",[$p1,$p2]];});};};var Prelude$prependToAll = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$prependToAll)(sep))(xs)));}throw ["unhandled case in prependToAll",[$p1,$p2]];});};};var Prelude$intercalate = function($p1){return function($p2){return new Fay$$$(function(){var xss = $p2;var xs = $p1;return Fay$$_(Prelude$concat)(Fay$$_(Fay$$_(Prelude$intersperse)(xs))(xss));});};};var Prelude$maximum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("maximum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude$foldl1)(Prelude$max))(xs);});};var Prelude$minimum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("minimum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude$foldl1)(Prelude$min))(xs);});};var Prelude$product = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("product: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(Fay$$mult))(1))(xs);});};var Prelude$sum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("sum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(Fay$$add))(0))(xs);});};var Prelude$scanl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var l = $p3;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(z))((function($tmp1){if (Fay$$_($tmp1) === null) {return null;}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var x = $tmp2.car;var xs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude$scanl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(l));});};};};var Prelude$scanl1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$scanl)(f))(x))(xs);}throw ["unhandled case in scanl1",[$p1,$p2]];});};};var Prelude$scanr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return Fay$$list([z]);}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude$$_undefined;})(Fay$$_(Fay$$_(Fay$$_(Prelude$scanr)(f))(z))(xs));}throw ["unhandled case in scanr",[$p1,$p2,$p3]];});};};};var Prelude$scanr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return Fay$$list([x]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude$$_undefined;})(Fay$$_(Fay$$_(Prelude$scanr1)(f))(xs));}throw ["unhandled case in scanr1",[$p1,$p2]];});};};var Prelude$lookup = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {var _key = $p1;return Prelude$Nothing;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var xys = $tmp1.cdr;var key = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(key))(x)) ? Fay$$_(Prelude$Just)(y) : Fay$$_(Fay$$_(Prelude$lookup)(key))(xys);}}throw ["unhandled case in lookup",[$p1,$p2]];});};};var Prelude$length = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Prelude$length$39$)(0))(xs);});};var Prelude$length$39$ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var acc = $p1;return Fay$$_(Fay$$_(Prelude$length$39$)(Fay$$_(Fay$$_(Fay$$add)(acc))(1)))(xs);}var acc = $p1;return acc;});};};var Prelude$reverse = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$reverse)(xs)))(Fay$$list([x]));}if (Fay$$_($p1) === null) {return null;}throw ["unhandled case in reverse",[$p1]];});};var Prelude$print = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["automatic"],$p1))));});};var Prelude$putStrLn = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs_string($p1))));});};var RingOscillator$Params = function(alpha){return function(omega){return function(deven){return function(dodd){return function(beta){return function(sigma){return function(nosc){return new Fay$$$(function(){return new $_RingOscillator$Params(alpha,omega,deven,dodd,beta,sigma,nosc);});};};};};};};};var RingOscillator$alpha = function(x){return new Fay$$$(function(){return Fay$$_(x).alpha;});};var RingOscillator$omega = function(x){return new Fay$$$(function(){return Fay$$_(x).omega;});};var RingOscillator$deven = function(x){return new Fay$$$(function(){return Fay$$_(x).deven;});};var RingOscillator$dodd = function(x){return new Fay$$$(function(){return Fay$$_(x).dodd;});};var RingOscillator$beta = function(x){return new Fay$$$(function(){return Fay$$_(x).beta;});};var RingOscillator$sigma = function(x){return new Fay$$$(function(){return Fay$$_(x).sigma;});};var RingOscillator$nosc = function(x){return new Fay$$$(function(){return Fay$$_(x).nosc;});};var RingOscillator$alphaUpd = function($p1){return function($p2){return new Fay$$$(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(p));$36$_record_to_update.alpha = val;return $36$_record_to_update;})();});};};var RingOscillator$omegaUpd = function($p1){return function($p2){return new Fay$$$(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(p));$36$_record_to_update.omega = val;return $36$_record_to_update;})();});};};var RingOscillator$devenUpd = function($p1){return function($p2){return new Fay$$$(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(p));$36$_record_to_update.deven = val;return $36$_record_to_update;})();});};};var RingOscillator$doddUpd = function($p1){return function($p2){return new Fay$$$(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(p));$36$_record_to_update.dodd = val;return $36$_record_to_update;})();});};};var RingOscillator$betaUpd = function($p1){return function($p2){return new Fay$$$(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(p));$36$_record_to_update.beta = val;return $36$_record_to_update;})();});};};var RingOscillator$sigmaUpd = function($p1){return function($p2){return new Fay$$$(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(p));$36$_record_to_update.sigma = val;return $36$_record_to_update;})();});};};var RingOscillator$noscUpd = function($p1){return function($p2){return new Fay$$$(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(p));$36$_record_to_update.nosc = val;return $36$_record_to_update;})();});};};var RingOscillator$defaultParams = new Fay$$$(function(){var Params = new $_RingOscillator$Params();Params.alpha = 1;Params.omega = 1.8;Params.deven = 7.5e-3;Params.dodd = 1.25e-2;Params.beta = 1;Params.sigma = 4;Params.nosc = 5;return Params;});var RingOscillator$initialState = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Fay$$_(Prelude$replicate)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(RingOscillator$nosc)(p)))(1)))(1)))(Fay$$_(Fay$$_(Prelude$replicate)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(RingOscillator$nosc)(p)))(1)))(0));});};var RingOscillator$ic = new Fay$$$(function(){return Fay$$_(Fay$$_(RingOscillator$centre)(RingOscillator$defaultParams))(Fay$$_(RingOscillator$initialState)(RingOscillator$defaultParams));});var RingOscillator$makeGraphData = function($p1){return function($p2){return new Fay$$$(function(){var size = $p2;var no = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$replicateM)(no))(Fay$$_(RingOscillator$newBuf)(size))))(function($p1){var bufs = $p1;return Fay$$_(Fay$$$_return)(Fay$$list([0,bufs]));});});};};var RingOscillator$rhs = function($p1){return function($p2){return new Fay$$$(function(){var state = $p2;var p = $p1;return (function(){var n = new Fay$$$(function(){return Fay$$_(RingOscillator$nosc)(p);});var ss = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$splitAt)(Fay$$_(Fay$$_(Fay$$add)(n))(1)))(state);});var sss = new Fay$$$(function(){return Fay$$_(Prelude$fst)(ss);});var sss$39$ = new Fay$$$(function(){return Fay$$_(Prelude$snd)(ss);});var y = new Fay$$$(function(){return Fay$$_(Prelude$head)(sss);});var xs = new Fay$$$(function(){return Fay$$_(Prelude$tail)(sss);});var y$39$ = new Fay$$$(function(){return Fay$$_(Prelude$head)(sss$39$);});var xs$39$ = new Fay$$$(function(){return Fay$$_(Prelude$tail)(sss$39$);});var dy = new Fay$$$(function(){return y$39$;});var dxs = new Fay$$$(function(){return xs$39$;});var dy$39$ = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$sub)(0))(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(RingOscillator$alpha)(p)))(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$$94$)(y))(2)))(1))))(y$39$))))(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Prelude$$94$)(Fay$$_(RingOscillator$omega)(p)))(2)))(y));});var xdiffs = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Prelude$head)(xs)))(Fay$$_(Fay$$_(Prelude$$33$$33$)(xs))(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))))(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(Fay$$sub))(Fay$$_(Prelude$tail)(xs)))(xs));});var vprime = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Fay$$_(Prelude$$94$)(x))(3));});};var vp = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$map)(vprime))(xdiffs);});var vfacs = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(Fay$$sub))(vp))(Fay$$_(Prelude$tail)(vp))))(Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$$33$$33$)(vp))(Fay$$_(Fay$$_(Fay$$sub)(n))(1))))(Fay$$_(Prelude$head)(vp))]));});var ds = new Fay$$$(function(){return Fay$$_(Prelude$cycle)(Fay$$list([Fay$$_(RingOscillator$dodd)(p),Fay$$_(RingOscillator$deven)(p)]));});var dxstmp = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith3)(function($p1){var d = $p1;return function($p2){var x$39$ = $p2;return function($p3){var vf = $p3;return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$sub)(0))(Fay$$_(Fay$$_(Fay$$mult)(d))(x$39$))))(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(RingOscillator$beta)(p)))(vf));};};}))(ds))(xs$39$))(vfacs);});var dxs$39$ = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Prelude$head)(dxstmp)))(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(RingOscillator$sigma)(p)))(y))))(Fay$$_(Prelude$tail)(dxstmp));});return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list([dy])))(Fay$$_(Fay$$_(Prelude$$43$$43$)(dxs))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list([dy$39$])))(dxs$39$)));})();});};};var RingOscillator$rk4 = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var h = $p3;var yn = $p2;var f = $p1;return (function(){var k1 = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$map)(mh)))(Fay$$_(f)(yn));});var k2 = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$map)(mh)))(Fay$$_(f)(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(Fay$$add))(yn))(Fay$$_(Fay$$_(Prelude$map)(half))(k1))));});var k3 = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$map)(mh)))(Fay$$_(f)(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(Fay$$add))(yn))(Fay$$_(Fay$$_(Prelude$map)(half))(k2))));});var k4 = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$map)(mh)))(Fay$$_(f)(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(Fay$$add))(yn))(k3)));});var mh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$mult)(h))(x);});};var half = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$mult)(0.5))(x);});};return Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$zipWith5)(function($p1){var y = $p1;return function($p2){var a = $p2;return function($p3){var b = $p3;return function($p4){var c = $p4;return function($p5){var d = $p5;return Fay$$_(Fay$$_(Fay$$add)(y))(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(a))(Fay$$_(Fay$$_(Fay$$mult)(2))(b))))(Fay$$_(Fay$$_(Fay$$mult)(2))(c))))(d)))(6));};};};};}))(yn))(k1))(k2))(k3))(k4);})();});};};};var RingOscillator$centre = function($p1){return function($p2){return new Fay$$$(function(){var s = $p2;var p = $p1;return (function(){var ss = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$splitAt)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(RingOscillator$nosc)(p)))(1)))(s);});var sss = new Fay$$$(function(){return Fay$$_(Prelude$fst)(ss);});var sss$39$ = new Fay$$$(function(){return Fay$$_(Prelude$snd)(ss);});var y = new Fay$$$(function(){return Fay$$_(Prelude$head)(sss);});var xs = new Fay$$$(function(){return Fay$$_(Prelude$tail)(sss);});var y$39$ = new Fay$$$(function(){return Fay$$_(Prelude$head)(sss$39$);});var xs$39$ = new Fay$$$(function(){return Fay$$_(Prelude$tail)(sss$39$);});var m = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude$sum)(xs)))(Fay$$_(Prelude$fromIntegral)(Fay$$_(Prelude$length)(xs)));});var xsc = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$map)(function($p1){var x = $p1;return Fay$$_(Fay$$_(Fay$$sub)(x))(m);}))(xs);});return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list([y])))(Fay$$_(Fay$$_(Prelude$$43$$43$)(xsc))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list([y$39$])))(xs$39$)));})();});};};var RingOscillator$ww = 400;var RingOscillator$wh = 400;var RingOscillator$gww = 640;var RingOscillator$gwh = 200;var RingOscillator$dt = 2.5e-2;var RingOscillator$framems = 30;var RingOscillator$renderrng = 6.0;var RingOscillator$rdotfac = 3.75e-2;var RingOscillator$rinner = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$wh))(Fay$$_(Fay$$_(Fay$$add)(2))(Fay$$_(Fay$$_(Fay$$mult)(7))(RingOscillator$rdotfac)));});var RingOscillator$rdot = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$mult)(RingOscillator$rdotfac))(RingOscillator$rinner);});var RingOscillator$fyoff = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$add)(RingOscillator$rinner))(Fay$$_(Fay$$_(Fay$$mult)(3))(RingOscillator$rdot));});var RingOscillator$ctr = new Fay$$$(function(){return Fay$$list([Fay$$_(Fay$$_(Fay$$mult)(0.5))(RingOscillator$ww),Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$mult)(5))(RingOscillator$rdot)))(RingOscillator$rinner)]);});var RingOscillator$gwbuf = 20;var RingOscillator$ticklen = 10;var RingOscillator$gwwtime = 10;var RingOscillator$pxpert = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$gww))(RingOscillator$gwwtime);});var RingOscillator$pxpersamp = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$mult)(RingOscillator$dt))(RingOscillator$pxpert);});var RingOscillator$nsamp = new Fay$$$(function(){return Fay$$_(Prelude$floor)(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(RingOscillator$gww))(RingOscillator$gwbuf)))(RingOscillator$pxpersamp));});var RingOscillator$main = new Fay$$$(function(){return Fay$$_(Fay$$_(RingOscillator$addWindowEventListener)(Fay$$list("load")))(RingOscillator$run);});var RingOscillator$run = function($p1){return new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$mapM)(RingOscillator$getElementById))(Fay$$list([Fay$$list("canvas"),Fay$$list("graph")]))))(function($p1){if (Fay$$listLen(Fay$$_($p1),2)) {var can = Fay$$index(0,Fay$$_($p1));var graph = Fay$$index(1,Fay$$_($p1));return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$mapM)(RingOscillator$getElementById))(Fay$$list([Fay$$list("go"),Fay$$list("stop"),Fay$$list("reset")]))))(function($p1){if (Fay$$listLen(Fay$$_($p1),3)) {var go = Fay$$index(0,Fay$$_($p1));var stop = Fay$$index(1,Fay$$_($p1));var reset = Fay$$index(2,Fay$$_($p1));return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$mapM)(RingOscillator$getElementById))(Fay$$list([Fay$$list("alpha"),Fay$$list("omega"),Fay$$list("deven"),Fay$$list("dodd"),Fay$$list("beta"),Fay$$list("sigma")]))))(function($p1){var sels = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$getElementById)(Fay$$list("nosc"))))(function($p1){var noscSel = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$mapM)(function($p1){var c = $p1;return Fay$$_(Fay$$_(RingOscillator$getContext)(c))(Fay$$list("2d"));}))(Fay$$list([can,graph]))))(function($p1){if (Fay$$listLen(Fay$$_($p1),2)) {var c = Fay$$index(0,Fay$$_($p1));var cg = Fay$$index(1,Fay$$_($p1));return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$newRef)(RingOscillator$defaultParams)))(function($p1){var pref = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$newRef)(RingOscillator$ic)))(function($p1){var xref = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$newRef)(Prelude$Nothing)))(function($p1){var timerref = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$makeGraphData)(Fay$$_(RingOscillator$nosc)(RingOscillator$defaultParams)))(RingOscillator$nsamp)))(function($p1){var gd = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$newRef)(gd)))(function($p1){var gdataref = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$render)(c))(RingOscillator$defaultParams))(RingOscillator$ic))(RingOscillator$renderrng)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$renderGraph)(cg))(pref))(gdataref))(Fay$$_(Fay$$_(RingOscillator$centre)(RingOscillator$defaultParams))(RingOscillator$ic)))(RingOscillator$renderrng)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(RingOscillator$addEventListener)(go))(Fay$$list("click"))))(Fay$$_(Fay$$_(Fay$$_(RingOscillator$doGo)(timerref))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$animate)(c))(cg))(pref))(xref))(gdataref))(RingOscillator$renderrng)))(RingOscillator$framems))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(RingOscillator$addEventListener)(stop))(Fay$$list("click"))))(Fay$$_(RingOscillator$doStop)(timerref))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(RingOscillator$addEventListener)(reset))(Fay$$list("click"))))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$doReset)(c))(cg))(timerref))(pref))(xref))(gdataref))(sels))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$forM_)(Fay$$_(Fay$$_(Prelude$zip)(sels))(Fay$$list([RingOscillator$alphaUpd,RingOscillator$omegaUpd,RingOscillator$devenUpd,RingOscillator$doddUpd,RingOscillator$betaUpd,RingOscillator$sigmaUpd])))))(function($p1){if (Fay$$listLen(Fay$$_($p1),2)) {var s = Fay$$index(0,Fay$$_($p1));var pfn = Fay$$index(1,Fay$$_($p1));return Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(RingOscillator$addEventListener)(s))(Fay$$list("change"))))(Fay$$_(Fay$$_(RingOscillator$doParamChange)(pref))(pfn));}throw ["unhandled case",$p1];})))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(RingOscillator$addEventListener)(noscSel))(Fay$$list("change"))))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$doNoscChange)(c))(cg))(timerref))(pref))(xref))(gdataref))))(Fay$$_(Fay$$$_return)(false))))))));});});});});});}throw ["unhandled case",$p1];});});});}throw ["unhandled case",$p1];});}throw ["unhandled case",$p1];});});};var RingOscillator$doGo = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var interval = $p3;var anim = $p2;var tref = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$readRef)(tref)))(function($p1){var oldtimer = $p1;return Fay$$_(Fay$$_(Fay$$then)((function($tmp1){if (Fay$$_($tmp1) instanceof $_Prelude$Nothing) {return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$setInterval)(anim))(interval)))(function($p1){var timer = $p1;return Fay$$_(Fay$$_(RingOscillator$writeRef)(tref))(Fay$$_(Prelude$Just)(timer));});}if (Fay$$_($tmp1) instanceof $_Prelude$Just) {return Fay$$_(Fay$$$_return)(Fay$$unit);}return (function(){ throw (["unhandled case",$tmp1]); })();})(oldtimer)))(Fay$$_(Fay$$$_return)(false));});});};};};};var RingOscillator$doStop = function($p1){return function($p2){return new Fay$$$(function(){var tref = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$readRef)(tref)))(function($p1){var oldtimer = $p1;return Fay$$_(Fay$$_(Fay$$then)((function($tmp1){if (Fay$$_($tmp1) instanceof $_Prelude$Nothing) {return Fay$$_(Fay$$$_return)(Fay$$unit);}if (Fay$$_($tmp1) instanceof $_Prelude$Just) {var timer = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(RingOscillator$clearInterval)(timer)))(Fay$$_(Fay$$_(RingOscillator$writeRef)(tref))(Prelude$Nothing));}return (function(){ throw (["unhandled case",$tmp1]); })();})(oldtimer)))(Fay$$_(Fay$$$_return)(false));});});};};var RingOscillator$doReset = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return function($p7){return function($p8){return new Fay$$$(function(){var e = $p8;var sels = $p7;var gdataref = $p6;var xref = $p5;var pref = $p4;var tref = $p3;var cg = $p2;var c = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$doStop)(tref))(e)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$writeRef)(pref))(RingOscillator$defaultParams)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$writeRef)(xref))(RingOscillator$ic)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$makeGraphData)(Fay$$_(RingOscillator$nosc)(RingOscillator$defaultParams)))(RingOscillator$nsamp)))(function($p1){var gd = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$writeRef)(gdataref))(gd)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$render)(c))(RingOscillator$defaultParams))(RingOscillator$ic))(RingOscillator$renderrng)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$renderGraph)(cg))(pref))(gdataref))(Fay$$_(Fay$$_(RingOscillator$centre)(RingOscillator$defaultParams))(RingOscillator$ic)))(RingOscillator$renderrng)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$forM_)(sels)))(function($p1){var s = $p1;return Fay$$_(Fay$$_(RingOscillator$setSelectIndex)(s))(2);})))(Fay$$_(Fay$$$_return)(false)))));}))));});};};};};};};};};var RingOscillator$doParamChange = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var e = $p3;var updfn = $p2;var pref = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$eventTarget)(e)))(function($p1){var target = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$selectValue)(target)))(function($p1){var sval = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$readRef)(pref)))(function($p1){var p = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$writeRef)(pref))(Fay$$_(Fay$$_(updfn)(p))(Fay$$_(RingOscillator$parseDouble)(sval)))))(Fay$$_(Fay$$$_return)(false));});});});});};};};var RingOscillator$doNoscChange = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return function($p7){return new Fay$$$(function(){var e = $p7;var gdataref = $p6;var xref = $p5;var pref = $p4;var timerref = $p3;var cg = $p2;var c = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$doStop)(timerref))(e)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$eventTarget)(e)))(function($p1){var target = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$selectValue)(target)))(function($p1){var sval = $p1;return (function(){var newNosc = new Fay$$$(function(){return Fay$$_(RingOscillator$parseInt)(sval);});return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$readRef)(pref)))(function($p1){var p = $p1;return (function(){var pnew = new Fay$$$(function(){var $36$_record_to_update = Object.create(Fay$$_(p));$36$_record_to_update.nosc = newNosc;return $36$_record_to_update;});return (function(){var xnew = new Fay$$$(function(){return Fay$$_(Fay$$_(RingOscillator$centre)(pnew))(Fay$$_(RingOscillator$initialState)(pnew));});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$writeRef)(pref))(pnew)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$writeRef)(xref))(xnew)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$makeGraphData)(newNosc))(RingOscillator$nsamp)))(function($p1){var gd = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$writeRef)(gdataref))(gd)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$render)(c))(pnew))(xnew))(RingOscillator$renderrng)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$renderGraph)(cg))(pref))(gdataref))(xnew))(RingOscillator$renderrng)))(Fay$$_(Fay$$$_return)(false))));})));})();})();});})();});}));});};};};};};};};var RingOscillator$animate = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return new Fay$$$(function(){var rng = $p6;var gdataref = $p5;var xref = $p4;var pref = $p3;var cg = $p2;var c = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$readRef)(pref)))(function($p1){var p = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$readRef)(xref)))(function($p1){var x = $p1;return (function(){var newx = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$_(RingOscillator$rk4)(Fay$$_(RingOscillator$rhs)(p)))(x))(RingOscillator$dt);});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$writeRef)(xref))(newx)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$render)(c))(p))(newx))(rng)))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$renderGraph)(cg))(pref))(gdataref))(Fay$$_(Fay$$_(RingOscillator$centre)(p))(newx)))(rng)));})();});});});};};};};};};var RingOscillator$render = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var rng = $p4;var s = $p3;var p = $p2;var c = $p1;return (function(){var dtickin = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$sub)(RingOscillator$fyoff))(Fay$$_(Fay$$_(Fay$$mult)(0.5))(RingOscillator$ticklen));});var dtickout = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$add)(RingOscillator$fyoff))(Fay$$_(Fay$$_(Fay$$mult)(0.5))(RingOscillator$ticklen));});return (function(){var f = new Fay$$$(function(){return Fay$$_(Prelude$head)(s);});return (function(){var xs = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$take)(Fay$$_(RingOscillator$nosc)(p))))(Fay$$_(Prelude$tail)(s));});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(RingOscillator$clearRect)(c))(Fay$$list([0,0])))(Fay$$list([RingOscillator$ww,RingOscillator$wh]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(RingOscillator$beginPath)(c)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$arc)(c))(RingOscillator$ctr))(RingOscillator$rinner))(0))(Fay$$_(Fay$$_(Fay$$mult)(2))(Prelude$pi))))((function(){var fscale = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$mult)(RingOscillator$rinner))(2)))(Prelude$pi)))(5);});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(RingOscillator$moveTo)(c)))(Fay$$_(Fay$$_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([(-(Fay$$_(Fay$$_(Fay$$_(Fay$$mult)(0.5))(fscale)))),(-(Fay$$_(RingOscillator$fyoff)))])))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(RingOscillator$lineTo)(c)))(Fay$$_(Fay$$_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([Fay$$_(Fay$$_(Fay$$mult)(0.5))(fscale),(-(Fay$$_(RingOscillator$fyoff)))])))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(RingOscillator$moveTo)(c)))(Fay$$_(Fay$$_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([0,(-(Fay$$_(dtickin)))])))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(RingOscillator$lineTo)(c)))(Fay$$_(Fay$$_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([0,(-(Fay$$_(dtickout)))])))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setLineWidth)(c))(2)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setStrokeStyle)(c))(Fay$$list("grey"))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(RingOscillator$stroke)(c)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$forM_)(Prelude$enumFromTo(0)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(RingOscillator$nosc)(p)))(1))))(function($p1){var i = $p1;return (function(){var th0 = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Prelude$fromIntegral)(i)))(2)))(Prelude$pi)))(Fay$$_(Prelude$fromIntegral)(Fay$$_(RingOscillator$nosc)(p)));});return (function(){var th = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$add)(th0))(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Prelude$$33$$33$)(xs))(i)))(rng)))(2)))(Prelude$pi)))(Fay$$_(Prelude$fromIntegral)(Fay$$_(RingOscillator$nosc)(p))));});return (function(){var dctr = new Fay$$$(function(){return Fay$$_(Fay$$_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([Fay$$_(Fay$$_(Fay$$mult)(RingOscillator$rinner))(Fay$$_(Prelude$sin)(th)),(-(Fay$$_(Fay$$_(Fay$$_(Fay$$mult)(RingOscillator$rinner))(Fay$$_(Prelude$cos)(th)))))]));});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(RingOscillator$beginPath)(c)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$arc)(c))(dctr))(RingOscillator$rdot))(0))(Fay$$_(Fay$$_(Fay$$mult)(2))(Prelude$pi))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setFillStyle)(c))(Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(i))(0)) ? Fay$$list("blue") : Fay$$list("red"))))(Fay$$_(RingOscillator$fill)(c))));})();})();})();})))((function(){var dctr = new Fay$$$(function(){return Fay$$_(Fay$$_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$divi)(f))(rng)))(fscale),(-(Fay$$_(RingOscillator$fyoff)))]));});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(RingOscillator$beginPath)(c)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$arc)(c))(dctr))(RingOscillator$rdot))(0))(Fay$$_(Fay$$_(Fay$$mult)(2))(Prelude$pi))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setFillStyle)(c))(Fay$$list("grey"))))(Fay$$_(RingOscillator$fill)(c))));})()))))))));})())));})();})();})();});};};};};var RingOscillator$renderGraph = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){var rng = $p5;var x = $p4;var gdataref = $p3;var pref = $p2;var cg = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$readRef)(pref)))(function($p1){var p = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$readRef)(gdataref)))(function($p1){if (Fay$$listLen(Fay$$_($p1),2)) {var ts = Fay$$index(0,Fay$$_($p1));var bufs = Fay$$index(1,Fay$$_($p1));return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setFont)(cg))(Fay$$list("10pt sans-serif"))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setStrokeStyle)(cg))(Fay$$list("grey"))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setFillStyle)(cg))(Fay$$list("grey"))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setLineWidth)(cg))(2)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(RingOscillator$clearRect)(cg))(Fay$$list([0,0])))(Fay$$list([RingOscillator$gww,RingOscillator$gwh]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(RingOscillator$beginPath)(cg)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$moveTo)(cg))(Fay$$list([0,Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$gwh))(2)]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$lineTo)(cg))(Fay$$list([RingOscillator$gww,Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$gwh))(2)]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$moveTo)(cg))(Fay$$list([1,0]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$lineTo)(cg))(Fay$$list([1,RingOscillator$gwh]))))((function(){var ticks = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$takeWhile)(function($p1){var t = $p1;return Fay$$_(Fay$$_(Fay$$lte)(t))(Fay$$_(Prelude$floor)(Fay$$_(Fay$$_(Fay$$add)(ts))(RingOscillator$gwwtime)));}))(Prelude$enumFrom(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Prelude$floor)(ts)))(1)));});return (function(){var tickxs = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$map)(function($p1){var t = $p1;return Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Prelude$fromIntegral)(t)))(ts)))(RingOscillator$pxpert);}))(ticks);});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$forM_)(Fay$$_(Fay$$_(Prelude$zip)(ticks))(tickxs))))(function($p1){if (Fay$$listLen(Fay$$_($p1),2)) {var t = Fay$$index(0,Fay$$_($p1));var x = Fay$$index(1,Fay$$_($p1));return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$moveTo)(cg))(Fay$$list([x,Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$gwh))(2)))(Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$ticklen))(2))]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$lineTo)(cg))(Fay$$list([x,Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$gwh))(2)))(Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$ticklen))(2))]))))((function(){var txt = new Fay$$$(function(){return Fay$$_(Prelude$show)(t);});return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$measureText)(cg))(txt)))(function($p1){var txtw = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$fillText)(cg))(txt))(Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(x))(Fay$$_(Fay$$_(Fay$$divi)(txtw))(2)),Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$gwh))(2)))(Fay$$_(Fay$$_(Fay$$mult)(2))(RingOscillator$ticklen))])))(Prelude$Nothing);});})()));}throw ["unhandled case",$p1];})))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(RingOscillator$stroke)(cg)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setLineWidth)(cg))(1)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(RingOscillator$forM)(Fay$$_(Fay$$_(Fay$$_(Prelude$zip3)(x))(bufs))(Fay$$_(Fay$$_(Fay$$cons)(Fay$$list("grey")))(Fay$$_(Fay$$_(Fay$$cons)(Fay$$list("blue")))(Fay$$_(Prelude$repeat)(Fay$$list("red"))))))))(function($p1){if (Fay$$listLen(Fay$$_($p1),3)) {var newx = Fay$$index(0,Fay$$_($p1));var buf = Fay$$index(1,Fay$$_($p1));var col = Fay$$index(2,Fay$$_($p1));return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$bufAdd)(buf))(newx)))(function($p1){var newbuf = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(RingOscillator$beginPath)(cg)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$setStrokeStyle)(cg))(col)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$bufVal)(newbuf))(0)))(function($p1){var y0 = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(RingOscillator$moveTo)(cg))(Fay$$list([0,Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$gwh))(2)))(Fay$$_(Fay$$_(Fay$$sub)(1))(Fay$$_(Fay$$_(Fay$$divi)(y0))(RingOscillator$renderrng)))]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$when)(Fay$$_(Fay$$_(Fay$$gt)(Fay$$_(RingOscillator$bufCurSize)(buf)))(1))))(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$forM_)(Prelude$enumFromTo(1)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(RingOscillator$bufCurSize)(buf)))(1)))))(function($p1){var i = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$bufVal)(newbuf))(i)))(function($p1){var y = $p1;return Fay$$_(Fay$$_(RingOscillator$lineTo)(cg))(Fay$$list([Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Prelude$fromIntegral)(i)))(RingOscillator$pxpersamp),Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$divi)(RingOscillator$gwh))(2)))(Fay$$_(Fay$$_(Fay$$sub)(1))(Fay$$_(Fay$$_(Fay$$divi)(y))(RingOscillator$renderrng)))]));});}))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(RingOscillator$stroke)(cg)))(Fay$$_(Fay$$$_return)(newbuf))));})));});}throw ["unhandled case",$p1];})))(function($p1){var newbufs = $p1;return Fay$$_(Fay$$_(RingOscillator$writeRef)(gdataref))(Fay$$list([Fay$$_(Fay$$_(Fay$$add)(ts))(Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(Fay$$_(RingOscillator$bufCurSize)(Fay$$_(Prelude$head)(newbufs))))(RingOscillator$nsamp)) ? 0 : RingOscillator$dt),newbufs]));}))));})();})()))))))))));}throw ["unhandled case",$p1];});});});};};};};};var RingOscillator$zipWith5 = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return new Fay$$$(function(){var $tmp1 = Fay$$_($p6);if ($tmp1 instanceof Fay$$Cons) {var e = $tmp1.car;var es = $tmp1.cdr;var $tmp1 = Fay$$_($p5);if ($tmp1 instanceof Fay$$Cons) {var d = $tmp1.car;var ds = $tmp1.cdr;var $tmp1 = Fay$$_($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var z = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(z)(a))(b))(c))(d))(e)))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$zipWith5)(z))(as))(bs))(cs))(ds))(es));}}}}}return null;});};};};};};};var RingOscillator$mapM = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(m)(x)))(function($p1){var mx = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$mapM)(m))(xs)))(function($p1){var mxs = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(mx))(mxs));});});}if (Fay$$_($p2) === null) {return Fay$$_(Fay$$$_return)(null);}throw ["unhandled case in mapM",[$p1,$p2]];});};};var RingOscillator$forM = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(m)(x)))(function($p1){var mx = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$mapM)(m))(xs)))(function($p1){var mxs = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(mx))(mxs));});});}if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(null);}throw ["unhandled case in forM",[$p1,$p2]];});};};var RingOscillator$replicateM = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var n = $p1;return Fay$$_(Prelude$sequence)(Fay$$_(Fay$$_(Prelude$replicate)(n))(x));});};};var RingOscillator$parseDouble = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(parseFloat(Fay$$fayToJs_string($p1)));});};var RingOscillator$parseInt = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(parseInt(Fay$$fayToJs_string($p1)));});};var RingOscillator$getElementById = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['getElementById'](Fay$$fayToJs_string($p1))));});};var RingOscillator$addWindowEventListener = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['addEventListener'](Fay$$fayToJs_string($p1),Fay$$fayToJs(["function",[["user","Event",[]],["action",[["bool"]]]]],$p2),false)));});};};var RingOscillator$addEventListener = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['addEventListener'](Fay$$fayToJs_string($p2),Fay$$fayToJs(["function",[["user","Event",[]],["action",[["bool"]]]]],$p3),false)));});};};};var RingOscillator$setInterval = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_int(window['setInterval'](Fay$$fayToJs(["action",[["unknown"]]],$p1),Fay$$fayToJs_double($p2))));});};};var RingOscillator$clearInterval = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['clearInterval'](Fay$$fayToJs_int($p1))));});};var RingOscillator$print = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],console.log(Fay$$fayToJs(["unknown"],$p1))));});};var RingOscillator$eventTarget = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],Fay$$fayToJs(["user","Event",[]],$p1)['target']));});};var RingOscillator$selectValue = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_string(Fay$$fayToJs(["user","Element",[]],$p1)[Fay$$fayToJs(["user","Element",[]],$p1)['selectedIndex']]['value']));});};var RingOscillator$setSelectIndex = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['selectedIndex']=Fay$$fayToJs_int($p2)));});};};var RingOscillator$newRef = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Ref",[["unknown"]]],new Fay$$Ref(Fay$$fayToJs(["unknown"],$p1))));});};var RingOscillator$writeRef = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$writeRef(Fay$$fayToJs(["user","Ref",[["unknown"]]],$p1),Fay$$fayToJs(["unknown"],$p2))));});};};var RingOscillator$readRef = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$readRef(Fay$$fayToJs(["user","Ref",[["unknown"]]],$p1))));});};var RingOscillator$getContext = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Context",[]],Fay$$fayToJs(["user","Element",[]],$p1).getContext(Fay$$fayToJs_string($p2))));});};};var RingOscillator$offset = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),2)) {var dx = Fay$$index(0,Fay$$_($p2));var dy = Fay$$index(1,Fay$$_($p2));if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(0,Fay$$_($p1));var y = Fay$$index(1,Fay$$_($p1));return Fay$$list([Fay$$_(Fay$$_(Fay$$add)(x))(dx),Fay$$_(Fay$$_(Fay$$add)(y))(dy)]);}}throw ["unhandled case in offset",[$p1,$p2]];});};};var RingOscillator$setFillStyle = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['fillStyle']=Fay$$fayToJs_string($p2)));});};};var RingOscillator$setFont = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['font']=Fay$$fayToJs_string($p2)));});};};var RingOscillator$setLineWidth = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['lineWidth']=Fay$$fayToJs_double($p2)));});};};var RingOscillator$setStrokeStyle = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['strokeStyle']=Fay$$fayToJs_string($p2)));});};};var RingOscillator$arc = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){var end = $p5;var beg = $p4;var r = $p3;if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var c = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$arc$39$)(c))(x))(y))(r))(beg))(end))(true);}throw ["unhandled case in arc",[$p1,$p2,$p3,$p4,$p5]];});};};};};};var RingOscillator$arcC = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){var end = $p5;var beg = $p4;var r = $p3;if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var c = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$arc$39$)(c))(x))(y))(r))(beg))(end))(false);}throw ["unhandled case in arcC",[$p1,$p2,$p3,$p4,$p5]];});};};};};};var RingOscillator$arc$39$ = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return function($p7){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['arc'](Fay$$fayToJs_double($p2),Fay$$fayToJs_double($p3),Fay$$fayToJs_double($p4),Fay$$fayToJs_double($p5),Fay$$fayToJs_double($p6),Fay$$fayToJs_bool($p7))));});};};};};};};};var RingOscillator$beginPath = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['beginPath']()));});};var RingOscillator$clip = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['clip']()));});};var RingOscillator$closePath = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['closePath']()));});};var RingOscillator$fill = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['fill']()));});};var RingOscillator$lineTo = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var c = $p1;return Fay$$_(Fay$$_(Fay$$_(RingOscillator$lineTo$39$)(c))(x))(y);}throw ["unhandled case in lineTo",[$p1,$p2]];});};};var RingOscillator$lineTo$39$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['lineTo'](Fay$$fayToJs_double($p2),Fay$$fayToJs_double($p3))));});};};};var RingOscillator$moveTo = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var c = $p1;return Fay$$_(Fay$$_(Fay$$_(RingOscillator$moveTo$39$)(c))(x))(y);}throw ["unhandled case in moveTo",[$p1,$p2]];});};};var RingOscillator$moveTo$39$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['moveTo'](Fay$$fayToJs_double($p2),Fay$$fayToJs_double($p3))));});};};};var RingOscillator$stroke = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['stroke']()));});};var RingOscillator$clearRect = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p3),2)) {var w = Fay$$index(0,Fay$$_($p3));var h = Fay$$index(1,Fay$$_($p3));if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var c = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$clearRect$39$)(c))(x))(y))(w))(h);}}throw ["unhandled case in clearRect",[$p1,$p2,$p3]];});};};};var RingOscillator$clearRect$39$ = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['clearRect'](Fay$$fayToJs_double($p2),Fay$$fayToJs_double($p3),Fay$$fayToJs_double($p4),Fay$$fayToJs_double($p5))));});};};};};};var RingOscillator$fillText = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){if (Fay$$_($p4) instanceof $_Prelude$Nothing) {if (Fay$$listLen(Fay$$_($p3),2)) {var x = Fay$$index(0,Fay$$_($p3));var y = Fay$$index(1,Fay$$_($p3));var s = $p2;var c = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$fillText1)(c))(s))(x))(y);}}if (Fay$$_($p4) instanceof $_Prelude$Just) {var mw = Fay$$_($p4).slot1;if (Fay$$listLen(Fay$$_($p3),2)) {var x = Fay$$index(0,Fay$$_($p3));var y = Fay$$index(1,Fay$$_($p3));var s = $p2;var c = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$fillText2)(c))(s))(x))(y))(mw);}}throw ["unhandled case in fillText",[$p1,$p2,$p3,$p4]];});};};};};var RingOscillator$fillText1 = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['fillText'](Fay$$fayToJs_string($p2),Fay$$fayToJs_double($p3),Fay$$fayToJs_double($p4))));});};};};};var RingOscillator$fillText2 = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['fillText'](Fay$$fayToJs_string($p2),Fay$$fayToJs_double($p3),Fay$$fayToJs_double($p4),Fay$$fayToJs_double($p5))));});};};};};};var RingOscillator$measureText = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_double(Fay$$fayToJs(["user","Context",[]],$p1)['measureText'](Fay$$fayToJs_string($p2))['width']));});};};var RingOscillator$Buffer = function(bufSize){return function(bufCurSize){return function(bufNext){return function(bufArr){return new Fay$$$(function(){return new $_RingOscillator$Buffer(bufSize,bufCurSize,bufNext,bufArr);});};};};};var RingOscillator$bufSize = function(x){return new Fay$$$(function(){return Fay$$_(x).bufSize;});};var RingOscillator$bufCurSize = function(x){return new Fay$$$(function(){return Fay$$_(x).bufCurSize;});};var RingOscillator$bufNext = function(x){return new Fay$$$(function(){return Fay$$_(x).bufNext;});};var RingOscillator$bufArr = function(x){return new Fay$$$(function(){return Fay$$_(x).bufArr;});};var RingOscillator$newBuf = function($p1){return new Fay$$$(function(){var size = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(RingOscillator$newArray)(size)))(function($p1){var arr = $p1;return Fay$$_(Fay$$_(Prelude$$36$)(Fay$$$_return))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$Buffer)(size))(0))(0))(arr));});});};var RingOscillator$bufAdd = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;if (Fay$$_($p1) instanceof $_RingOscillator$Buffer) {var sz = Fay$$_($p1).bufSize;var cursz = Fay$$_($p1).bufCurSize;var nxt = Fay$$_($p1).bufNext;var arr = Fay$$_($p1).bufArr;return (function(){var cursz$39$ = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(cursz))(sz)) ? Fay$$_(Fay$$_(Fay$$add)(cursz))(1) : sz;});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(RingOscillator$setArrayVal)(arr))(nxt))(x)))((function(){var nxt$39$ = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$rem)(Fay$$_(Fay$$_(Fay$$add)(nxt))(1)))(sz);});return Fay$$_(Fay$$_(Prelude$$36$)(Fay$$$_return))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(RingOscillator$Buffer)(sz))(cursz$39$))(nxt$39$))(arr));})());})();}throw ["unhandled case in bufAdd",[$p1,$p2]];});};};var RingOscillator$bufVal = function($p1){return function($p2){return new Fay$$$(function(){var i = $p2;if (Fay$$_($p1) instanceof $_RingOscillator$Buffer) {var sz = Fay$$_($p1).bufSize;var cursz = Fay$$_($p1).bufCurSize;var nxt = Fay$$_($p1).bufNext;var arr = Fay$$_($p1).bufArr;return (function(){var idx = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$rem)(Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(cursz))(sz)) ? i : Fay$$_(Fay$$_(Fay$$add)(nxt))(i)))(sz);});return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(RingOscillator$arrayVal)(arr))(idx)))(Fay$$$_return);})();}throw ["unhandled case in bufVal",[$p1,$p2]];});};};var RingOscillator$newArray = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Array",[]],new Array(Fay$$fayToJs_int($p1))));});};var RingOscillator$setArrayVal = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Array",[]],$p1)[Fay$$fayToJs_int($p2)]=Fay$$fayToJs_double($p3)));});};};};var RingOscillator$arrayVal = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_double(Fay$$fayToJs(["user","Array",[]],$p1)[Fay$$fayToJs_int($p2)]));});};};var $_Language$Fay$FFI$Nullable = function(slot1){this.slot1 = slot1;};var $_Language$Fay$FFI$Null = function(){};var $_Language$Fay$FFI$Defined = function(slot1){this.slot1 = slot1;};var $_Language$Fay$FFI$Undefined = function(){};var $_Language$Fay$FFI$Nullable = function(slot1){this.slot1 = slot1;};var $_Language$Fay$FFI$Null = function(){};var $_Language$Fay$FFI$Defined = function(slot1){this.slot1 = slot1;};var $_Language$Fay$FFI$Undefined = function(){};var $_Prelude$Just = function(slot1){this.slot1 = slot1;};var $_Prelude$Nothing = function(){};var $_Prelude$Left = function(slot1){this.slot1 = slot1;};var $_Prelude$Right = function(slot1){this.slot1 = slot1;};var $_Prelude$Ratio = function(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};var $_Prelude$GT = function(){};var $_Prelude$LT = function(){};var $_Prelude$EQ = function(){};var $_RingOscillator$Params = function(alpha,omega,deven,dodd,beta,sigma,nosc){this.alpha = alpha;this.omega = omega;this.deven = deven;this.dodd = dodd;this.beta = beta;this.sigma = sigma;this.nosc = nosc;};var $_RingOscillator$Buffer = function(bufSize,bufCurSize,bufNext,bufArr){this.bufSize = bufSize;this.bufCurSize = bufCurSize;this.bufNext = bufNext;this.bufArr = bufArr;};var Fay$$fayToJsUserDefined = function(type,obj){var _obj = Fay$$_(obj);var argTypes = type[2];if (_obj instanceof $_Language$Fay$FFI$Nullable) {var obj_ = {"instance": "Nullable"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Null) {var obj_ = {"instance": "Null"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Defined) {var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Undefined) {var obj_ = {"instance": "Undefined"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Nullable) {var obj_ = {"instance": "Nullable"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Null) {var obj_ = {"instance": "Null"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Defined) {var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Undefined) {var obj_ = {"instance": "Undefined"};return obj_;}if (_obj instanceof $_Prelude$Just) {var obj_ = {"instance": "Just"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Prelude$Nothing) {var obj_ = {"instance": "Nothing"};return obj_;}if (_obj instanceof $_Prelude$Left) {var obj_ = {"instance": "Left"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Prelude$Right) {var obj_ = {"instance": "Right"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Prelude$Ratio) {var obj_ = {"instance": "Ratio"};var obj_slot1 = Fay$$fayToJs_int(_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs_int(_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;}if (_obj instanceof $_Prelude$GT) {var obj_ = {"instance": "GT"};return obj_;}if (_obj instanceof $_Prelude$LT) {var obj_ = {"instance": "LT"};return obj_;}if (_obj instanceof $_Prelude$EQ) {var obj_ = {"instance": "EQ"};return obj_;}if (_obj instanceof $_RingOscillator$Params) {var obj_ = {"instance": "Params"};var obj_alpha = Fay$$fayToJs_double(_obj.alpha);if (undefined !== obj_alpha) {obj_['alpha'] = obj_alpha;}var obj_omega = Fay$$fayToJs_double(_obj.omega);if (undefined !== obj_omega) {obj_['omega'] = obj_omega;}var obj_deven = Fay$$fayToJs_double(_obj.deven);if (undefined !== obj_deven) {obj_['deven'] = obj_deven;}var obj_dodd = Fay$$fayToJs_double(_obj.dodd);if (undefined !== obj_dodd) {obj_['dodd'] = obj_dodd;}var obj_beta = Fay$$fayToJs_double(_obj.beta);if (undefined !== obj_beta) {obj_['beta'] = obj_beta;}var obj_sigma = Fay$$fayToJs_double(_obj.sigma);if (undefined !== obj_sigma) {obj_['sigma'] = obj_sigma;}var obj_nosc = Fay$$fayToJs_int(_obj.nosc);if (undefined !== obj_nosc) {obj_['nosc'] = obj_nosc;}return obj_;}if (_obj instanceof $_RingOscillator$Buffer) {var obj_ = {"instance": "Buffer"};var obj_bufSize = Fay$$fayToJs_int(_obj.bufSize);if (undefined !== obj_bufSize) {obj_['bufSize'] = obj_bufSize;}var obj_bufCurSize = Fay$$fayToJs_int(_obj.bufCurSize);if (undefined !== obj_bufCurSize) {obj_['bufCurSize'] = obj_bufCurSize;}var obj_bufNext = Fay$$fayToJs_int(_obj.bufNext);if (undefined !== obj_bufNext) {obj_['bufNext'] = obj_bufNext;}var obj_bufArr = Fay$$fayToJs(["user","Array",[]],_obj.bufArr);if (undefined !== obj_bufArr) {obj_['bufArr'] = obj_bufArr;}return obj_;}return obj;};var Fay$$jsToFayUserDefined = function(type,obj){var argTypes = type[2];if (obj["instance"] === "Nullable") {return new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Null") {return new $_Language$Fay$FFI$Null();}if (obj["instance"] === "Defined") {return new $_Language$Fay$FFI$Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Undefined") {return new $_Language$Fay$FFI$Undefined();}if (obj["instance"] === "Nullable") {return new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Null") {return new $_Language$Fay$FFI$Null();}if (obj["instance"] === "Defined") {return new $_Language$Fay$FFI$Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Undefined") {return new $_Language$Fay$FFI$Undefined();}if (obj["instance"] === "Just") {return new $_Prelude$Just(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Nothing") {return new $_Prelude$Nothing();}if (obj["instance"] === "Left") {return new $_Prelude$Left(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Right") {return new $_Prelude$Right(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Ratio") {return new $_Prelude$Ratio(Fay$$jsToFay_int(obj["slot1"]),Fay$$jsToFay_int(obj["slot2"]));}if (obj["instance"] === "GT") {return new $_Prelude$GT();}if (obj["instance"] === "LT") {return new $_Prelude$LT();}if (obj["instance"] === "EQ") {return new $_Prelude$EQ();}if (obj["instance"] === "Params") {return new $_RingOscillator$Params(Fay$$jsToFay_double(obj["alpha"]),Fay$$jsToFay_double(obj["omega"]),Fay$$jsToFay_double(obj["deven"]),Fay$$jsToFay_double(obj["dodd"]),Fay$$jsToFay_double(obj["beta"]),Fay$$jsToFay_double(obj["sigma"]),Fay$$jsToFay_int(obj["nosc"]));}if (obj["instance"] === "Buffer") {return new $_RingOscillator$Buffer(Fay$$jsToFay_int(obj["bufSize"]),Fay$$jsToFay_int(obj["bufCurSize"]),Fay$$jsToFay_int(obj["bufNext"]),Fay$$jsToFay(["user","Array",[]],obj["bufArr"]));}return obj;};
// Exports
this.RingOscillator$Buffer = RingOscillator$Buffer;
this.RingOscillator$Params = RingOscillator$Params;
this.RingOscillator$main = RingOscillator$main;

// Built-ins
this._ = Fay$$_;
this.$           = Fay$$$;
this.$fayToJs    = Fay$$fayToJs;
this.$jsToFay    = Fay$$jsToFay;

};
;
var main = new RingOscillator();
main._(main.RingOscillator$main);

