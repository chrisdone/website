// Generated by psc-bundle 0.10.0
var PS = {};
(function(exports) {
    "use strict";

  exports.arrayMap = function (f) {
    return function (arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
    "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
    "use strict";

  exports.showNumberImpl = function (n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Data.Show"];     
  var Show = function (show) {
      this.show = show;
  };                                                 
  var showNumber = new Show($foreign.showNumberImpl);
  var show = function (dict) {
      return dict.show;
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["showNumber"] = showNumber;
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Data.Unit"];
  var Data_Show = PS["Data.Show"];
  exports["unit"] = $foreign.unit;
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var Semigroupoid = function (compose) {
      this.compose = compose;
  };
  var semigroupoidFn = new Semigroupoid(function (f) {
      return function (g) {
          return function (x) {
              return f(g(x));
          };
      };
  });
  var compose = function (dict) {
      return dict.compose;
  };
  exports["Semigroupoid"] = Semigroupoid;
  exports["compose"] = compose;
  exports["semigroupoidFn"] = semigroupoidFn;
})(PS["Control.Semigroupoid"] = PS["Control.Semigroupoid"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Data.Functor"];
  var Data_Function = PS["Data.Function"];
  var Data_Unit = PS["Data.Unit"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];        
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };                                                                                             
  var functorArray = new Functor($foreign.arrayMap);
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["functorArray"] = functorArray;
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var Control_Semigroupoid = PS["Control.Semigroupoid"];        
  var Category = function (__superclass_Control$dotSemigroupoid$dotSemigroupoid_0, id) {
      this["__superclass_Control.Semigroupoid.Semigroupoid_0"] = __superclass_Control$dotSemigroupoid$dotSemigroupoid_0;
      this.id = id;
  };
  var id = function (dict) {
      return dict.id;
  };
  var categoryFn = new Category(function () {
      return Control_Semigroupoid.semigroupoidFn;
  }, function (x) {
      return x;
  });
  exports["Category"] = Category;
  exports["id"] = id;
  exports["categoryFn"] = categoryFn;
})(PS["Control.Category"] = PS["Control.Category"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Function = PS["Data.Function"];
  var Control_Category = PS["Control.Category"];        
  var Apply = function (__superclass_Data$dotFunctor$dotFunctor_0, apply) {
      this["__superclass_Data.Functor.Functor_0"] = __superclass_Data$dotFunctor$dotFunctor_0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var Control_Apply = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Applicative = function (__superclass_Control$dotApply$dotApply_0, pure) {
      this["__superclass_Control.Apply.Apply_0"] = __superclass_Control$dotApply$dotApply_0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["liftA1"] = liftA1;
  exports["pure"] = pure;
})(PS["Control.Applicative"] = PS["Control.Applicative"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];        
  var Bind = function (__superclass_Control$dotApply$dotApply_0, bind) {
      this["__superclass_Control.Apply.Apply_0"] = __superclass_Control$dotApply$dotApply_0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Functor = PS["Data.Functor"];        
  var Monad = function (__superclass_Control$dotApplicative$dotApplicative_0, __superclass_Control$dotBind$dotBind_1) {
      this["__superclass_Control.Applicative.Applicative_0"] = __superclass_Control$dotApplicative$dotApplicative_0;
      this["__superclass_Control.Bind.Bind_1"] = __superclass_Control$dotBind$dotBind_1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(f)(function (v) {
                  return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS["Control.Monad"] = PS["Control.Monad"] || {});
(function(exports) {
    "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Control.Monad.Eff"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad = PS["Control.Monad"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var monadEff = new Control_Monad.Monad(function () {
      return applicativeEff;
  }, function () {
      return bindEff;
  });
  var bindEff = new Control_Bind.Bind(function () {
      return applyEff;
  }, $foreign.bindE);
  var applyEff = new Control_Apply.Apply(function () {
      return functorEff;
  }, Control_Monad.ap(monadEff));
  var applicativeEff = new Control_Applicative.Applicative(function () {
      return applyEff;
  }, $foreign.pureE);
  var functorEff = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEff));
  exports["functorEff"] = functorEff;
  exports["applyEff"] = applyEff;
  exports["applicativeEff"] = applicativeEff;
  exports["bindEff"] = bindEff;
  exports["monadEff"] = monadEff;
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
    "use strict";

  // module Control.Monad.Eff.Console

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  exports["log"] = $foreign.log;
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
    "use strict";

  exports.newRef = function (val) {
    return function () {
      return { value: val };
    };
  };

  exports.readRef = function (ref) {
    return function () {
      return ref.value;
    };
  };

  exports.writeRef = function (ref) {
    return function (val) {
      return function () {
        ref.value = val;
        return {};
      };
    };
  };
})(PS["Control.Monad.Eff.Ref"] = PS["Control.Monad.Eff.Ref"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Ref"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Unit = PS["Data.Unit"];
  exports["newRef"] = $foreign.newRef;
  exports["readRef"] = $foreign.readRef;
  exports["writeRef"] = $foreign.writeRef;
})(PS["Control.Monad.Eff.Ref"] = PS["Control.Monad.Eff.Ref"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.setInterval = function (ms) {
    return function (fn) {
      return function () {
        return setInterval(fn, ms);
      };
    };
  };
})(PS["Control.Monad.Eff.Timer"] = PS["Control.Monad.Eff.Timer"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Timer"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Ord = PS["Data.Ord"];
  exports["setInterval"] = $foreign.setInterval;
})(PS["Control.Monad.Eff.Timer"] = PS["Control.Monad.Eff.Timer"] || {});
(function(exports) {
    "use strict";

  exports.foldrArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };

  exports.foldlArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };
})(PS["Data.Foldable"] = PS["Data.Foldable"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Data.Semigroup"];
  var Data_Unit = PS["Data.Unit"];
  var Data_Void = PS["Data.Void"];                         
  var append = function (dict) {
      return dict.append;
  };
  exports["append"] = append;
})(PS["Data.Semigroup"] = PS["Data.Semigroup"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var Data_Function = PS["Data.Function"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Unit = PS["Data.Unit"];
  var mempty = function (dict) {
      return dict.mempty;
  };
  exports["mempty"] = mempty;
})(PS["Data.Monoid"] = PS["Data.Monoid"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Extend = PS["Control.Extend"];
  var Control_Monad = PS["Control.Monad"];
  var Control_MonadZero = PS["Control.MonadZero"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Bounded = PS["Data.Bounded"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var Control_Category = PS["Control.Category"];        
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  exports["Just"] = Just;
  exports["Nothing"] = Nothing;
})(PS["Data.Maybe"] = PS["Data.Maybe"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Data.Foldable"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Plus = PS["Control.Plus"];
  var Data_BooleanAlgebra = PS["Data.BooleanAlgebra"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Maybe_First = PS["Data.Maybe.First"];
  var Data_Maybe_Last = PS["Data.Maybe.Last"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Monoid_Additive = PS["Data.Monoid.Additive"];
  var Data_Monoid_Conj = PS["Data.Monoid.Conj"];
  var Data_Monoid_Disj = PS["Data.Monoid.Disj"];
  var Data_Monoid_Dual = PS["Data.Monoid.Dual"];
  var Data_Monoid_Endo = PS["Data.Monoid.Endo"];
  var Data_Monoid_Multiplicative = PS["Data.Monoid.Multiplicative"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Semiring = PS["Data.Semiring"];
  var Data_Unit = PS["Data.Unit"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Control_Category = PS["Control.Category"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_HeytingAlgebra = PS["Data.HeytingAlgebra"];        
  var Foldable = function (foldMap, foldl, foldr) {
      this.foldMap = foldMap;
      this.foldl = foldl;
      this.foldr = foldr;
  };
  var foldr = function (dict) {
      return dict.foldr;
  };
  var foldl = function (dict) {
      return dict.foldl;
  }; 
  var foldMapDefaultR = function (dictFoldable) {
      return function (dictMonoid) {
          return function (f) {
              return function (xs) {
                  return foldr(dictFoldable)(function (x) {
                      return function (acc) {
                          return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(f(x))(acc);
                      };
                  })(Data_Monoid.mempty(dictMonoid))(xs);
              };
          };
      };
  };
  var foldableArray = new Foldable(function (dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
  }, $foreign.foldlArray, $foreign.foldrArray);
  var foldMap = function (dict) {
      return dict.foldMap;
  };
  exports["Foldable"] = Foldable;
  exports["foldMap"] = foldMap;
  exports["foldMapDefaultR"] = foldMapDefaultR;
  exports["foldl"] = foldl;
  exports["foldr"] = foldr;
  exports["foldableArray"] = foldableArray;
})(PS["Data.Foldable"] = PS["Data.Foldable"] || {});
(function(exports) {
    "use strict";

  // module Data.Traversable

  // jshint maxparams: 3

  exports.traverseArrayImpl = function () {
    function Cont(fn) {
      this.fn = fn;
    }

    var emptyList = {};

    var ConsCell = function (head, tail) {
      this.head = head;
      this.tail = tail;
    };

    function consList(x) {
      return function (xs) {
        return new ConsCell(x, xs);
      };
    }

    function listToArray(list) {
      var arr = [];
      while (list !== emptyList) {
        arr.push(list.head);
        list = list.tail;
      }
      return arr;
    }

    return function (apply) {
      return function (map) {
        return function (pure) {
          return function (f) {
            var buildFrom = function (x, ys) {
              return apply(map(consList)(f(x)))(ys);
            };

            var go = function (acc, currentLen, xs) {
              if (currentLen === 0) {
                return acc;
              } else {
                var last = xs[currentLen - 1];
                return new Cont(function () {
                  return go(buildFrom(last, acc), currentLen - 1, xs);
                });
              }
            };

            return function (array) {
              var result = go(pure(emptyList), array.length, array);
              while (result instanceof Cont) {
                result = result.fn();
              }

              return map(listToArray)(result);
            };
          };
        };
      };
    };
  }();
})(PS["Data.Traversable"] = PS["Data.Traversable"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Data.Traversable"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Maybe_First = PS["Data.Maybe.First"];
  var Data_Maybe_Last = PS["Data.Maybe.Last"];
  var Data_Monoid_Additive = PS["Data.Monoid.Additive"];
  var Data_Monoid_Conj = PS["Data.Monoid.Conj"];
  var Data_Monoid_Disj = PS["Data.Monoid.Disj"];
  var Data_Monoid_Dual = PS["Data.Monoid.Dual"];
  var Data_Monoid_Multiplicative = PS["Data.Monoid.Multiplicative"];
  var Traversable = function (__superclass_Data$dotFoldable$dotFoldable_1, __superclass_Data$dotFunctor$dotFunctor_0, sequence, traverse) {
      this["__superclass_Data.Foldable.Foldable_1"] = __superclass_Data$dotFoldable$dotFoldable_1;
      this["__superclass_Data.Functor.Functor_0"] = __superclass_Data$dotFunctor$dotFunctor_0;
      this.sequence = sequence;
      this.traverse = traverse;
  };
  var traverse = function (dict) {
      return dict.traverse;
  };
  var sequenceDefault = function (dictTraversable) {
      return function (dictApplicative) {
          return function (tma) {
              return traverse(dictTraversable)(dictApplicative)(Control_Category.id(Control_Category.categoryFn))(tma);
          };
      };
  };
  var traversableArray = new Traversable(function () {
      return Data_Foldable.foldableArray;
  }, function () {
      return Data_Functor.functorArray;
  }, function (dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
  }, function (dictApplicative) {
      return $foreign.traverseArrayImpl(Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]()))(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]()))(Control_Applicative.pure(dictApplicative));
  });
  var sequence = function (dict) {
      return dict.sequence;
  };
  exports["Traversable"] = Traversable;
  exports["sequence"] = sequence;
  exports["sequenceDefault"] = sequenceDefault;
  exports["traverse"] = traverse;
  exports["traversableArray"] = traversableArray;
})(PS["Data.Traversable"] = PS["Data.Traversable"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
      return function() {
          var el = document.getElementById(id);
          if (el && el instanceof HTMLCanvasElement) {
              return Just(el);
          } else {
              return Nothing;
          }
      };
  };

  exports.getContext2D = function(c) {
      return function() {
          return c.getContext('2d');
      };
  };

  exports.getCanvasWidth = function(canvas) {
      return function() {
          return canvas.width;
      };
  };

  exports.setFillStyle = function(style) {
      return function(ctx) {
          return function() {
              ctx.fillStyle = style;
              return ctx;
          };
      };
  };

  exports.beginPath = function(ctx) {
      return function() {
          ctx.beginPath();
          return ctx;
      };
  };

  exports.stroke = function(ctx) {
      return function() {
          ctx.stroke();
          return ctx;
      };
  };

  exports.fill = function(ctx) {
      return function() {
          ctx.fill();
          return ctx;
      };
  };

  exports.closePath = function(ctx) {
      return function() {
          ctx.closePath();
          return ctx;
      };
  };

  exports.arc = function(ctx) {
      return function(a) {
          return function() {
              ctx.arc(a.x, a.y, a.r, a.start, a.end);
              return ctx;
          };
      };
  };

  exports.clearRect = function(ctx) {
      return function(r) {
          return function() {
              ctx.clearRect(r.x, r.y, r.w, r.h);
              return ctx;
          };
      };
  };

  exports.save = function(ctx) {
      return function() {
          ctx.save();
          return ctx;
      };
  };

  exports.restore = function(ctx) {
      return function() {
          ctx.restore();
          return ctx;
      };
  };
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Graphics.Canvas"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Exception_Unsafe = PS["Control.Monad.Eff.Exception.Unsafe"];
  var Data_ArrayBuffer_Types = PS["Data.ArrayBuffer.Types"];
  var Data_Function_Uncurried = PS["Data.Function.Uncurried"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Show = PS["Data.Show"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Function = PS["Data.Function"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Functor = PS["Data.Functor"];
  var getCanvasElementById = function (elId) {
      return $foreign.getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
  };
  exports["getCanvasElementById"] = getCanvasElementById;
  exports["arc"] = $foreign.arc;
  exports["beginPath"] = $foreign.beginPath;
  exports["clearRect"] = $foreign.clearRect;
  exports["closePath"] = $foreign.closePath;
  exports["fill"] = $foreign.fill;
  exports["getCanvasWidth"] = $foreign.getCanvasWidth;
  exports["getContext2D"] = $foreign.getContext2D;
  exports["restore"] = $foreign.restore;
  exports["save"] = $foreign.save;
  exports["setFillStyle"] = $foreign.setFillStyle;
  exports["stroke"] = $foreign.stroke;
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
    "use strict";              

  exports.pi = Math.PI;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var $foreign = PS["Math"];
  exports["pi"] = $foreign.pi;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by psc version 0.10.0
  "use strict";
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var Control_Monad_Eff_Timer = PS["Control.Monad.Eff.Timer"];
  var Data_Array = PS["Data.Array"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Traversable = PS["Data.Traversable"];
  var Graphics_Canvas = PS["Graphics.Canvas"];
  var $$Math = PS["Math"];
  var Prelude = PS["Prelude"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Semiring = PS["Data.Semiring"];
  var Control_Applicative = PS["Control.Applicative"];
  var Data_Unit = PS["Data.Unit"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Eq = PS["Data.Eq"];
  var Data_HeytingAlgebra = PS["Data.HeytingAlgebra"];
  var Data_Ord = PS["Data.Ord"];
  var Data_EuclideanRing = PS["Data.EuclideanRing"];        
  var circle = function (ctx) {
      return function (c) {
          return function __do() {
              Graphics_Canvas.beginPath(ctx)();
              Graphics_Canvas.setFillStyle(c.c)(ctx)();
              Graphics_Canvas.arc(ctx)({
                  x: c.x, 
                  y: c.y, 
                  r: c.r, 
                  start: 0.0, 
                  end: $$Math.pi * 2.0
              })();
              Graphics_Canvas.stroke(ctx)();
              Graphics_Canvas.fill(ctx)();
              return Graphics_Canvas.closePath(ctx)();
          };
      };
  };
  var main = function __do() {
      var v = Graphics_Canvas.getCanvasElementById("canvas")();
      if (v instanceof Data_Maybe.Nothing) {
          return Data_Unit.unit;
      };
      if (v instanceof Data_Maybe.Just) {
          var v1 = Graphics_Canvas.getCanvasWidth(v.value0)();
          var v2 = Graphics_Canvas.getCanvasWidth(v.value0)();
          Control_Monad_Eff_Console.log(Data_Show.show(Data_Show.showNumber)(v1) + (" " + Data_Show.show(Data_Show.showNumber)(v2)))();
          var v3 = Graphics_Canvas.getContext2D(v.value0)();
          var v4 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Control_Monad_Eff.applicativeEff)(function (x) {
              return Control_Monad_Eff_Ref.newRef(x);
          })([ {
              i: 0, 
              x: 20.0, 
              y: 30.0, 
              r: 5.0, 
              xv: 2.0, 
              yv: 0.0, 
              c: "#ff90ff", 
              mass: 1.0
          }, {
              i: 1, 
              x: 80.0, 
              y: 50.0, 
              r: 5.0, 
              xv: -5.0, 
              yv: 0.0, 
              c: "#55ff00", 
              mass: 1.0
          }, {
              i: 2, 
              x: 20.0, 
              y: 50.0, 
              r: 5.0, 
              xv: 1.0, 
              yv: 0.0, 
              c: "#ff0000", 
              mass: 1.0
          }, {
              i: 3, 
              x: 50.0, 
              y: 50.0, 
              r: 5.0, 
              xv: -4.0, 
              yv: 7.5e-2, 
              c: "#00ffaa", 
              mass: 1.0
          }, {
              i: 4, 
              x: 20.0, 
              y: 100.0, 
              r: 5.0, 
              xv: 5.0, 
              yv: 0.0, 
              c: "#0000ff", 
              mass: 1.0
          }, {
              i: 5, 
              x: 85.0, 
              y: 90.0, 
              r: 10.0, 
              xv: -3.0, 
              yv: 0.0, 
              c: "#ff00ff", 
              mass: 2.0
          }, {
              i: 6, 
              x: 65.0, 
              y: 70.0, 
              r: 5.0, 
              xv: 0.1, 
              yv: 2.0, 
              c: "#ffff00", 
              mass: 1.0
          }, {
              i: 7, 
              x: 30.0, 
              y: 30.0, 
              r: 5.0, 
              xv: -5.0e-2, 
              yv: 2.5, 
              c: "#00ffff", 
              mass: 1.0
          } ])();
          Control_Monad_Eff_Timer.setInterval(25)(function __do() {
              Graphics_Canvas.clearRect(v3)({
                  x: 0.0, 
                  y: 0.0, 
                  w: v1, 
                  h: v2
              })();
              Graphics_Canvas.save(v3)();
              var v5 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Control_Monad_Eff.applicativeEff)(Control_Monad_Eff_Ref.readRef)(v4)();
              Data_Traversable.traverse(Data_Traversable.traversableArray)(Control_Monad_Eff.applicativeEff)(function (ref) {
                  return function __do() {
                      var v6 = Control_Monad_Eff_Ref.readRef(ref)();
                      var ball$prime = Data_Foldable.foldl(Data_Foldable.foldableArray)(function (a0) {
                          return function (b) {
                              var ydist = (a0.y + a0.yv) - (b.y + b.yv);
                              var xdist = (a0.x + a0.xv) - (b.x + b.xv);
                              var distSquared = xdist * xdist + ydist * ydist;
                              var $15 = a0.i === b.i;
                              if ($15) {
                                  return a0;
                              };
                              if (!$15) {
                                  var a = (function () {
                                      var $18 = {};
                                      for (var $19 in a0) {
                                          if (a0.hasOwnProperty($19)) {
                                              $18[$19] = a0[$19];
                                          };
                                      };
                                      $18.xv = (function () {
                                          var $16 = (a0.x + a0.xv) - a0.r < 0.0 || a0.x + a0.xv + a0.r > v1;
                                          if ($16) {
                                              return -a0.xv;
                                          };
                                          if (!$16) {
                                              return a0.xv;
                                          };
                                          throw new Error("Failed pattern match at Main line 68, column 48 - line 70, column 58: " + [ $16.constructor.name ]);
                                      })();
                                      $18.yv = (function () {
                                          var $17 = (a0.y + a0.yv) - a0.r < 0.0 || a0.y + a0.yv + a0.r > v2;
                                          if ($17) {
                                              return -a0.yv;
                                          };
                                          if (!$17) {
                                              return a0.yv;
                                          };
                                          throw new Error("Failed pattern match at Main line 71, column 48 - line 73, column 58: " + [ $17.constructor.name ]);
                                      })();
                                      return $18;
                                  })();
                                  var $21 = distSquared <= (a.r + b.r) * (a.r + b.r);
                                  if ($21) {
                                      var yvelocity = b.yv - a.yv;
                                      var xvelocity = b.xv - a.xv;
                                      var dotProduct = xdist * xvelocity + ydist * yvelocity;
                                      var $22 = dotProduct > 0.0;
                                      if ($22) {
                                          var combinedMass = a.mass + b.mass;
                                          var collisionScale = dotProduct / distSquared;
                                          var xcol = xdist * collisionScale;
                                          var ycol = ydist * collisionScale;
                                          var colWeightB = (2.0 * a.mass) / combinedMass;
                                          var colWeightA = (2.0 * b.mass) / combinedMass;
                                          var $23 = {};
                                          for (var $24 in a) {
                                              if (a.hasOwnProperty($24)) {
                                                  $23[$24] = a[$24];
                                              };
                                          };
                                          $23.xv = a.xv + colWeightA * xcol;
                                          $23.yv = a.yv + colWeightA * ycol;
                                          return $23;
                                      };
                                      if (!$22) {
                                          return a;
                                      };
                                      throw new Error("Failed pattern match at Main line 80, column 46 - line 91, column 55: " + [ $22.constructor.name ]);
                                  };
                                  if (!$21) {
                                      return a;
                                  };
                                  throw new Error("Failed pattern match at Main line 76, column 35 - line 92, column 44: " + [ $21.constructor.name ]);
                              };
                              throw new Error("Failed pattern match at Main line 63, column 30 - line 92, column 44: " + [ $15.constructor.name ]);
                          };
                      })(v6)(v5);
                      var ball$prime$prime = (function () {
                          var $26 = {};
                          for (var $27 in ball$prime) {
                              if (ball$prime.hasOwnProperty($27)) {
                                  $26[$27] = ball$prime[$27];
                              };
                          };
                          $26.x = ball$prime.x + ball$prime.xv;
                          $26.y = ball$prime.y + ball$prime.yv;
                          return $26;
                      })();
                      Control_Monad_Eff_Ref.writeRef(ref)(ball$prime$prime)();
                      return circle(v3)(ball$prime$prime)();
                  };
              })(v4)();
              Graphics_Canvas.restore(v3)();
              return Data_Unit.unit;
          })();
          return Data_Unit.unit;
      };
      throw new Error("Failed pattern match at Main line 26, column 3 - line 107, column 16: " + [ v.constructor.name ]);
  };
  exports["circle"] = circle;
  exports["main"] = main;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();