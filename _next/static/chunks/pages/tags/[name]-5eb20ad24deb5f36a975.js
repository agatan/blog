(self.webpackChunk_N_E=self.webpackChunk_N_E||[]).push([[357],{4003:function(t,r,e){"use strict";e.d(r,{i:function(){return u}});var n=e(63),i=e(2326),o=e(5284),c=e(3105),a=e(4461),s=e(3808),d=e(7294);function l(){return(l=Object.assign||function(t){for(var r=1;r<arguments.length;r++){var e=arguments[r];for(var n in e)Object.prototype.hasOwnProperty.call(e,n)&&(t[n]=e[n])}return t}).apply(this,arguments)}function h(t,r){if(null==t)return{};var e,n,i={},o=Object.keys(t);for(n=0;n<o.length;n++)e=o[n],r.indexOf(e)>=0||(i[e]=t[e]);return i}var u=(0,n.G)(((t,r)=>{var e=(0,i.m)("Divider",t),{borderLeftWidth:n,borderBottomWidth:s,borderTopWidth:u,borderRightWidth:p,borderWidth:x,borderStyle:g,borderColor:f}=e,j=h(e,["borderLeftWidth","borderBottomWidth","borderTopWidth","borderRightWidth","borderWidth","borderStyle","borderColor"]),m=(0,o.Lr)(t),{className:b,orientation:v="horizontal",__css:y}=m,w=h(m,["className","orientation","__css"]),W={vertical:{borderLeftWidth:n||p||x||"1px",height:"100%"},horizontal:{borderBottomWidth:s||u||x||"1px",width:"100%"}};return d.createElement(c.m$.hr,l({ref:r,"aria-orientation":v},w,{__css:l({},j,{border:"0",borderColor:f,borderStyle:g},W[v],y),className:(0,a.cx)("chakra-divider",b)}))}));s.Ts&&(u.displayName="Divider")},6359:function(t,r,e){"use strict";e.d(r,{r:function(){return d}});var n=e(6265),i=e(5893),o=(e(7294),e(5063)),c=e(9444);function a(t,r){var e=Object.keys(t);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(t);r&&(n=n.filter((function(r){return Object.getOwnPropertyDescriptor(t,r).enumerable}))),e.push.apply(e,n)}return e}function s(t){for(var r=1;r<arguments.length;r++){var e=null!=arguments[r]?arguments[r]:{};r%2?a(Object(e),!0).forEach((function(r){(0,n.Z)(t,r,e[r])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(e)):a(Object(e)).forEach((function(r){Object.defineProperty(t,r,Object.getOwnPropertyDescriptor(e,r))}))}return t}var d=function(t){return(0,i.jsx)(o.default,{href:t.href,children:(0,i.jsx)(c.r,s(s({},t),{},{onClick:function(r){null!=t.onClick&&t.onClick(r),document.activeElement.blur()}}))})}},8436:function(t,r,e){"use strict";e.d(r,{Z:function(){return g}});var n=e(5893),i=(e(7294),e(8017)),o=e(3850),c=e(4096),a=e(336),s=e(4115),d=e(7341),l=e(58),h=e(3880),u=e(6034),p=e(9583),x=e(6359),g=function(t){return(0,n.jsxs)(i.xu,{backgroundColor:"green.50",minHeight:"100vh",children:[(0,n.jsx)(o.W,{maxWidth:"container.lg",backgroundColor:"green.600",children:(0,n.jsxs)(c.k,{align:"flex-end",padding:"8",children:[(0,n.jsx)(a.X,{children:(0,n.jsx)(x.r,{href:"/",color:"white",children:(0,n.jsx)(s.x,{as:"u",children:"\u2197 agatan blog \u2197"})})}),(0,n.jsx)(d.L,{}),(0,n.jsxs)(i.xu,{children:[(0,n.jsx)(x.r,{href:"/feed.xml",padding:"2",children:(0,n.jsx)(l.J,{as:p.FZ6,color:"white",boxSize:"6"})}),(0,n.jsx)(x.r,{href:"/tags",padding:"2",children:(0,n.jsx)(l.J,{as:p.YxP,color:"white",boxSize:"6"})})]})]})}),(0,n.jsx)(o.W,{maxWidth:"container.lg",backgroundColor:"white",minHeight:"100vh",children:(0,n.jsxs)(c.k,{paddingTop:"4",paddingBottom:"4",children:[(0,n.jsx)(o.W,{maxWidth:"container.lg",padding:"0",margin:"0",children:t.children}),(0,n.jsx)(o.W,{maxWidth:"44",children:(0,n.jsxs)(c.k,{direction:"column",align:"center",padding:"4",children:[(0,n.jsxs)(x.r,{href:"https://twitter.com/@agatan_",children:[(0,n.jsx)(i.xu,{position:"relative",width:"20",height:"20",children:(0,n.jsx)(h.E,{src:"/i/agatan.png",layout:"fill",objectFit:"cover",alt:"agatan"})}),(0,n.jsx)(s.x,{fontSize:"large",align:"center",children:"@agatan"})]}),(0,n.jsxs)(u.Ug,{children:[(0,n.jsx)(x.r,{href:"https://twitter.com/@agatan_",padding:"1",children:(0,n.jsx)(l.J,{as:p.fWC,focusable:!0,boxSize:"1.2em"})}),(0,n.jsx)(x.r,{href:"https://github.com/agatan",padding:"1",children:(0,n.jsx)(l.J,{as:p.hJX,focusable:!0,boxSize:"1.2em"})})]})]})})]})})]})}},8201:function(t,r,e){"use strict";e.d(r,{p:function(){return h}});var n=e(5893),i=(e(7294),e(3850)),o=e(6034),c=e(336),a=e(4115),s=e(6359),d=e(608),l=function(t){var r=t.postMeta;return(0,n.jsxs)(i.W,{maxW:"container.md",children:[(0,n.jsx)(c.X,{size:"md",children:(0,n.jsx)(s.r,{href:"/posts/".concat(r.slug),children:r.title})}),(0,n.jsx)(a.x,{color:"gray.800",children:new Date(r.timestamp).toDateString()}),(0,n.jsx)(o.Ug,{paddingTop:"4",paddingBottom:"4",spacing:"0",children:r.tags.map((function(t){return(0,n.jsx)(d.k,{tag:t},"".concat(r.slug,"-").concat(t))}))})]})},h=function(t){var r=t.postMetas;return(0,n.jsx)(i.W,{maxW:"container.md",children:(0,n.jsx)(o.gC,{divider:(0,n.jsx)(o.cX,{}),spacing:4,paddingTop:"4",paddingBottom:"4",align:"stretch",children:r.map((function(t){return(0,n.jsx)(l,{postMeta:t},t.slug)}))})})}},6053:function(t,r,e){"use strict";e.d(r,{H:function(){return o}});var n=e(5893),i=e(9008),o=(e(7294),function(t){var r=t.title,e=t.description;return(0,n.jsxs)(i.default,{children:[(0,n.jsx)("title",{children:"".concat(r," | ").concat("\u53f3\u4e0a\u2197")}),e&&(0,n.jsx)("meta",{name:"description",content:e}),(0,n.jsx)("meta",{property:"og:type",content:"website"}),(0,n.jsx)("meta",{property:"og:title",content:r}),e&&(0,n.jsx)("meta",{property:"og:description",content:e}),(0,n.jsx)("meta",{property:"og:site_name",content:"\u53f3\u4e0a\u2197"}),(0,n.jsx)("meta",{property:"twitter:card",content:"summary"}),(0,n.jsx)("meta",{property:"twitter:creator",content:"@agatan_"}),(0,n.jsx)("meta",{property:"twitter:title",content:r}),e&&(0,n.jsx)("meta",{property:"twitter:description",content:e})]})})},608:function(t,r,e){"use strict";e.d(r,{k:function(){return s}});var n=e(5893),i=e(5528),o=e(4096),c=e(4115),a=(e(7294),e(6359)),s=function(t){var r=t.tag,e=t.count;return(0,n.jsx)(a.r,{href:"/tags/".concat(r),padding:"1",children:(0,n.jsx)(i.Vp,{colorScheme:"blue",variant:"outline",padding:"1",children:(0,n.jsxs)(o.k,{align:"center",paddingLeft:"1",paddingRight:"1",children:[(0,n.jsx)(i.Sn,{fontSize:"small",children:r}),null!=e&&(0,n.jsx)(c.x,{fontSize:"x-small",color:"blue.500",paddingLeft:"1",children:e})]})})})}},9123:function(t,r,e){"use strict";e.r(r),e.d(r,{__N_SSG:function(){return l}});var n=e(5893),i=(e(7294),e(3850)),o=e(336),c=e(4003),a=e(8201),s=e(6053),d=e(8436),l=!0;r.default=function(t){var r=t.tag,e=t.postMetas;return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(s.H,{title:"#".concat(r),description:"#".concat(r,"\u306b\u95a2\u3059\u308b\u6295\u7a3f")}),(0,n.jsxs)(d.Z,{children:[(0,n.jsxs)(i.W,{maxW:"container.md",children:[(0,n.jsxs)(o.X,{paddingBottom:"4",fontSize:"3xl",color:"blue.500",children:["#",r]}),(0,n.jsx)(c.i,{})]}),(0,n.jsx)(a.p,{postMetas:e})]})]})}},1329:function(t,r,e){(window.__NEXT_P=window.__NEXT_P||[]).push(["/tags/[name]",function(){return e(9123)}])}},function(t){t.O(0,[445,191,774,888,179],(function(){return r=1329,t(t.s=r);var r}));var r=t.O();_N_E=r}]);