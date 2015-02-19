function print_int(n) {
	console._stdout.write(""+n);
}
var print_string = print_int;

var Format = {
	level:0,
	levels:[],
	fprintf: function(fp,s) {
		var n = 2;
		var arg = arguments;
		return fp(s.replace(/(%.|@\[<([^>])>|@\n|@.)/g, function(a, b ,c) {
			switch (b.substr(0,2)) {
				case "%a":
					var f = arg[n++];
					return f(function(e){return e;},arg[n++]);
				case "%f":
					return arg[n++].toFixed(6);
				case "@.":
					return "\n";
				case "@?":
					return "";
				case "@[":
					var level = c|0;
					Format.level+=level;
					Format.levels.push(level);
					return "";
				case "@]":
					Format.level -= Format.levels.pop();
					return "";
				case "@\n":
					var str = "\n";
					for(var i = 0; i < Format.level;i++) str += " ";
					return str;

				default: return arg[n++];
			}

		}));
	},
	sprintf: function(s) {
		var args = Array.prototype.slice.call(arguments);
	    args.unshift(function(i){return i;});
		return Format.fprintf.apply(this,args);
	},
	printf: function() {
		var args = Array.prototype.slice.call(arguments);
	    args.unshift(print_string);
		Format.fprintf.apply(this,args);
	},
};

  Format.printf("@[<2>tes(");
    Format.printf("@\n// test@\n@[<2>tes(");
      Format.printf("@\n@[<2>tes(");
        Format.printf("@\naa");
        Format.printf("@\naa");
        Format.printf("@\naa");
      Format.printf("@]@\n)");
    Format.printf("@]@\n)");
  Format.printf("@]@\n)");
  Format.printf("@.");
