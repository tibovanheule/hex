(this["webpackJsonphex-website"]=this["webpackJsonphex-website"]||[]).push([[0],{18:function(t,e,n){},19:function(t,e,n){"use strict";n.r(e);var a=n(0),r=n.n(a),o=n(4),i=n.n(o),s=n(5),c=n(6),u=n(1),l=n(8),d=n(7),h=n(2),v=function(t){Object(l.a)(n,t);var e=Object(d.a)(n);function n(t){var a;return Object(s.a)(this,n),(a=e.call(this,t)).state={svg:void 0},a.post=a.post.bind(Object(u.a)(a)),a.makeBoard=a.makeBoard.bind(Object(u.a)(a)),a.list=a.list.bind(Object(u.a)(a)),a}return Object(c.a)(n,[{key:"componentDidMount",value:function(){this.post("size: 5 * 5\nturn: blue\ntiles: 0\nstate: undecided\norientation: red * blue")}},{key:"post",value:function(t){var e=this;fetch("https://hex.tibovanheule.space/move/svg",{method:"POST",headers:{"Content-Type":"text/plain"},body:t}).then((function(t){return t.text()})).then((function(t){return e.setState({svg:t})}))}},{key:"makeBoard",value:function(t,e){var n=String.fromCharCode(parseInt(t)+65);e=parseInt(e)+1;var a=this.state.coor.map((function(t){return console.log(t),"    (".concat(String.fromCharCode(t[0]+65)).concat(t[1],") -> ").concat(t[2])}));this.post("size: 5 * 5\n        turn: blue\n        tiles: ".concat(this.state.coor.length+1,"\n        ").concat(a.join("\n"),"\n           (").concat(n).concat(e,") -> red\n        state: undecided\n        orientation: red * blue"))}},{key:"list",value:function(t){var e=Array.from(t.getElementsByTagName("use")).filter((function(t){return t.hasAttribute("fill")})).map((function(t){return[parseInt(t.getAttribute("x")),parseInt(t.getAttribute("y"))+1,t.getAttribute("fill")]}));this.setState({coor:e})}},{key:"render",value:function(){var t=this;return r.a.createElement("div",null,void 0===this.state.svg&&r.a.createElement("p",null,"LOADING"),void 0!==this.state.svg&&r.a.createElement(h.SvgLoader,{svgXML:this.state.svg,onSVGReady:this.list},r.a.createElement(h.SvgProxy,{selector:"use",onClick:function(e){return t.makeBoard(e.target.x.baseVal.value,e.target.y.baseVal.value)}})))}}]),n}(a.Component);n(18);i.a.render(r.a.createElement(v,null),document.getElementById("root"))},9:function(t,e,n){t.exports=n(19)}},[[9,1,2]]]);
//# sourceMappingURL=main.339afb44.chunk.js.map