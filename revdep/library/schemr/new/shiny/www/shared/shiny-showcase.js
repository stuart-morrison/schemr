/*! shiny 1.10.0 | (c) 2012-2024 RStudio, PBC. | License: GPL-3 | file LICENSE */
"use strict";(function(){var ta=Object.create;var Re=Object.defineProperty;var na=Object.getOwnPropertyDescriptor;var ia=Object.getOwnPropertyNames;var aa=Object.getPrototypeOf,oa=Object.prototype.hasOwnProperty;var i=function(r,e){return function(){return e||r((e={exports:{}}).exports,e),e.exports}};var ua=function(r,e,t,n){if(e&&typeof e=="object"||typeof e=="function")for(var a=ia(e),o=0,l=a.length,u;o<l;o++)u=a[o],!oa.call(r,u)&&u!==t&&Re(r,u,{get:function(s){return e[s]}.bind(null,u),enumerable:!(n=na(e,u))||n.enumerable});return r};var la=function(r,e,t){return t=r!=null?ta(aa(r)):{},ua(e||!r||!r.__esModule?Re(t,"default",{value:r,enumerable:!0}):t,r)};var q=i(function(nv,_e){var W=function(r){return r&&r.Math==Math&&r};_e.exports=W(typeof globalThis=="object"&&globalThis)||W(typeof window=="object"&&window)||W(typeof self=="object"&&self)||W(typeof global=="object"&&global)||function(){return this}()||Function("return this")()});var g=i(function(iv,je){je.exports=function(r){try{return!!r()}catch(e){return!0}}});var S=i(function(av,Ne){var ca=g();Ne.exports=!ca(function(){return Object.defineProperty({},1,{get:function(){return 7}})[1]!=7})});var H=i(function(ov,De){var va=g();De.exports=!va(function(){var r=function(){}.bind();return typeof r!="function"||r.hasOwnProperty("prototype")})});var C=i(function(uv,Ae){var sa=H(),z=Function.prototype.call;Ae.exports=sa?z.bind(z):function(){return z.apply(z,arguments)}});var Fe=i(function($e){"use strict";var Be={}.propertyIsEnumerable,Me=Object.getOwnPropertyDescriptor,fa=Me&&!Be.call({1:2},1);$e.f=fa?function(e){var t=Me(this,e);return!!t&&t.enumerable}:Be});var mr=i(function(cv,Le){Le.exports=function(r,e){return{enumerable:!(r&1),configurable:!(r&2),writable:!(r&4),value:e}}});var p=i(function(vv,Ke){var Ue=H(),Ge=Function.prototype,br=Ge.call,da=Ue&&Ge.bind.bind(br,br);Ke.exports=Ue?da:function(r){return function(){return br.apply(r,arguments)}}});var M=i(function(sv,We){var ke=p(),pa=ke({}.toString),ga=ke("".slice);We.exports=function(r){return ga(pa(r),8,-1)}});var ze=i(function(fv,He){var ya=p(),qa=g(),ha=M(),xr=Object,ma=ya("".split);He.exports=qa(function(){return!xr("z").propertyIsEnumerable(0)})?function(r){return ha(r)=="String"?ma(r,""):xr(r)}:xr});var Y=i(function(dv,Ye){Ye.exports=function(r){return r==null}});var F=i(function(pv,Xe){var ba=Y(),xa=TypeError;Xe.exports=function(r){if(ba(r))throw xa("Can't call method on "+r);return r}});var L=i(function(gv,Ve){var Ea=ze(),Sa=F();Ve.exports=function(r){return Ea(Sa(r))}});var Sr=i(function(yv,Je){var Er=typeof document=="object"&&document.all,Oa=typeof Er=="undefined"&&Er!==void 0;Je.exports={all:Er,IS_HTMLDDA:Oa}});var y=i(function(qv,Qe){var Ze=Sr(),Ia=Ze.all;Qe.exports=Ze.IS_HTMLDDA?function(r){return typeof r=="function"||r===Ia}:function(r){return typeof r=="function"}});var _=i(function(hv,tt){var rt=y(),et=Sr(),wa=et.all;tt.exports=et.IS_HTMLDDA?function(r){return typeof r=="object"?r!==null:rt(r)||r===wa}:function(r){return typeof r=="object"?r!==null:rt(r)}});var X=i(function(mv,nt){var Or=q(),Ta=y(),Pa=function(r){return Ta(r)?r:void 0};nt.exports=function(r,e){return arguments.length<2?Pa(Or[r]):Or[r]&&Or[r][e]}});var at=i(function(bv,it){var Ca=p();it.exports=Ca({}.isPrototypeOf)});var ut=i(function(xv,ot){ot.exports=typeof navigator!="undefined"&&String(navigator.userAgent)||""});var pt=i(function(Ev,dt){var ft=q(),Ir=ut(),lt=ft.process,ct=ft.Deno,vt=lt&&lt.versions||ct&&ct.version,st=vt&&vt.v8,b,V;st&&(b=st.split("."),V=b[0]>0&&b[0]<4?1:+(b[0]+b[1]));!V&&Ir&&(b=Ir.match(/Edge\/(\d+)/),(!b||b[1]>=74)&&(b=Ir.match(/Chrome\/(\d+)/),b&&(V=+b[1])));dt.exports=V});var wr=i(function(Sv,yt){var gt=pt(),Ra=g();yt.exports=!!Object.getOwnPropertySymbols&&!Ra(function(){var r=Symbol();return!String(r)||!(Object(r)instanceof Symbol)||!Symbol.sham&&gt&&gt<41})});var Tr=i(function(Ov,qt){var _a=wr();qt.exports=_a&&!Symbol.sham&&typeof Symbol.iterator=="symbol"});var Pr=i(function(Iv,ht){var ja=X(),Na=y(),Da=at(),Aa=Tr(),Ba=Object;ht.exports=Aa?function(r){return typeof r=="symbol"}:function(r){var e=ja("Symbol");return Na(e)&&Da(e.prototype,Ba(r))}});var bt=i(function(wv,mt){var Ma=String;mt.exports=function(r){try{return Ma(r)}catch(e){return"Object"}}});var Et=i(function(Tv,xt){var $a=y(),Fa=bt(),La=TypeError;xt.exports=function(r){if($a(r))return r;throw La(Fa(r)+" is not a function")}});var Cr=i(function(Pv,St){var Ua=Et(),Ga=Y();St.exports=function(r,e){var t=r[e];return Ga(t)?void 0:Ua(t)}});var It=i(function(Cv,Ot){var Rr=C(),_r=y(),jr=_(),Ka=TypeError;Ot.exports=function(r,e){var t,n;if(e==="string"&&_r(t=r.toString)&&!jr(n=Rr(t,r))||_r(t=r.valueOf)&&!jr(n=Rr(t,r))||e!=="string"&&_r(t=r.toString)&&!jr(n=Rr(t,r)))return n;throw Ka("Can't convert object to primitive value")}});var Tt=i(function(Rv,wt){wt.exports=!1});var J=i(function(_v,Ct){var Pt=q(),ka=Object.defineProperty;Ct.exports=function(r,e){try{ka(Pt,r,{value:e,configurable:!0,writable:!0})}catch(t){Pt[r]=e}return e}});var Z=i(function(jv,_t){var Wa=q(),Ha=J(),Rt="__core-js_shared__",za=Wa[Rt]||Ha(Rt,{});_t.exports=za});var Q=i(function(Nv,Nt){var Ya=Tt(),jt=Z();(Nt.exports=function(r,e){return jt[r]||(jt[r]=e!==void 0?e:{})})("versions",[]).push({version:"3.29.0",mode:Ya?"pure":"global",copyright:"\xA9 2014-2023 Denis Pushkarev (zloirock.ru)",license:"https://github.com/zloirock/core-js/blob/v3.29.0/LICENSE",source:"https://github.com/zloirock/core-js"})});var Nr=i(function(Dv,Dt){var Xa=F(),Va=Object;Dt.exports=function(r){return Va(Xa(r))}});var I=i(function(Av,At){var Ja=p(),Za=Nr(),Qa=Ja({}.hasOwnProperty);At.exports=Object.hasOwn||function(e,t){return Qa(Za(e),t)}});var Dr=i(function(Bv,Bt){var ro=p(),eo=0,to=Math.random(),no=ro(1 .toString);Bt.exports=function(r){return"Symbol("+(r===void 0?"":r)+")_"+no(++eo+to,36)}});var N=i(function(Mv,$t){var io=q(),ao=Q(),Mt=I(),oo=Dr(),uo=wr(),lo=Tr(),j=io.Symbol,Ar=ao("wks"),co=lo?j.for||j:j&&j.withoutSetter||oo;$t.exports=function(r){return Mt(Ar,r)||(Ar[r]=uo&&Mt(j,r)?j[r]:co("Symbol."+r)),Ar[r]}});var Gt=i(function($v,Ut){var vo=C(),Ft=_(),Lt=Pr(),so=Cr(),fo=It(),po=N(),go=TypeError,yo=po("toPrimitive");Ut.exports=function(r,e){if(!Ft(r)||Lt(r))return r;var t=so(r,yo),n;if(t){if(e===void 0&&(e="default"),n=vo(t,r,e),!Ft(n)||Lt(n))return n;throw go("Can't convert object to primitive value")}return e===void 0&&(e="number"),fo(r,e)}});var Br=i(function(Fv,Kt){var qo=Gt(),ho=Pr();Kt.exports=function(r){var e=qo(r,"string");return ho(e)?e:e+""}});var $r=i(function(Lv,Wt){var mo=q(),kt=_(),Mr=mo.document,bo=kt(Mr)&&kt(Mr.createElement);Wt.exports=function(r){return bo?Mr.createElement(r):{}}});var Fr=i(function(Uv,Ht){var xo=S(),Eo=g(),So=$r();Ht.exports=!xo&&!Eo(function(){return Object.defineProperty(So("div"),"a",{get:function(){return 7}}).a!=7})});var Lr=i(function(Yt){var Oo=S(),Io=C(),wo=Fe(),To=mr(),Po=L(),Co=Br(),Ro=I(),_o=Fr(),zt=Object.getOwnPropertyDescriptor;Yt.f=Oo?zt:function(e,t){if(e=Po(e),t=Co(t),_o)try{return zt(e,t)}catch(n){}if(Ro(e,t))return To(!Io(wo.f,e,t),e[t])}});var Ur=i(function(Kv,Xt){var jo=S(),No=g();Xt.exports=jo&&No(function(){return Object.defineProperty(function(){},"prototype",{value:42,writable:!1}).prototype!=42})});var w=i(function(kv,Vt){var Do=_(),Ao=String,Bo=TypeError;Vt.exports=function(r){if(Do(r))return r;throw Bo(Ao(r)+" is not an object")}});var U=i(function(Zt){var Mo=S(),$o=Fr(),Fo=Ur(),rr=w(),Jt=Br(),Lo=TypeError,Gr=Object.defineProperty,Uo=Object.getOwnPropertyDescriptor,Kr="enumerable",kr="configurable",Wr="writable";Zt.f=Mo?Fo?function(e,t,n){if(rr(e),t=Jt(t),rr(n),typeof e=="function"&&t==="prototype"&&"value"in n&&Wr in n&&!n[Wr]){var a=Uo(e,t);a&&a[Wr]&&(e[t]=n.value,n={configurable:kr in n?n[kr]:a[kr],enumerable:Kr in n?n[Kr]:a[Kr],writable:!1})}return Gr(e,t,n)}:Gr:function(e,t,n){if(rr(e),t=Jt(t),rr(n),$o)try{return Gr(e,t,n)}catch(a){}if("get"in n||"set"in n)throw Lo("Accessors not supported");return"value"in n&&(e[t]=n.value),e}});var er=i(function(Hv,Qt){var Go=S(),Ko=U(),ko=mr();Qt.exports=Go?function(r,e,t){return Ko.f(r,e,ko(1,t))}:function(r,e,t){return r[e]=t,r}});var tn=i(function(zv,en){var Hr=S(),Wo=I(),rn=Function.prototype,Ho=Hr&&Object.getOwnPropertyDescriptor,zr=Wo(rn,"name"),zo=zr&&function(){}.name==="something",Yo=zr&&(!Hr||Hr&&Ho(rn,"name").configurable);en.exports={EXISTS:zr,PROPER:zo,CONFIGURABLE:Yo}});var an=i(function(Yv,nn){var Xo=p(),Vo=y(),Yr=Z(),Jo=Xo(Function.toString);Vo(Yr.inspectSource)||(Yr.inspectSource=function(r){return Jo(r)});nn.exports=Yr.inspectSource});var ln=i(function(Xv,un){var Zo=q(),Qo=y(),on=Zo.WeakMap;un.exports=Qo(on)&&/native code/.test(String(on))});var Xr=i(function(Vv,vn){var ru=Q(),eu=Dr(),cn=ru("keys");vn.exports=function(r){return cn[r]||(cn[r]=eu(r))}});var tr=i(function(Jv,sn){sn.exports={}});var Qr=i(function(Zv,pn){var tu=ln(),dn=q(),nu=_(),iu=er(),Vr=I(),Jr=Z(),au=Xr(),ou=tr(),fn="Object already initialized",Zr=dn.TypeError,uu=dn.WeakMap,nr,G,ir,lu=function(r){return ir(r)?G(r):nr(r,{})},cu=function(r){return function(e){var t;if(!nu(e)||(t=G(e)).type!==r)throw Zr("Incompatible receiver, "+r+" required");return t}};tu||Jr.state?(x=Jr.state||(Jr.state=new uu),x.get=x.get,x.has=x.has,x.set=x.set,nr=function(r,e){if(x.has(r))throw Zr(fn);return e.facade=r,x.set(r,e),e},G=function(r){return x.get(r)||{}},ir=function(r){return x.has(r)}):(R=au("state"),ou[R]=!0,nr=function(r,e){if(Vr(r,R))throw Zr(fn);return e.facade=r,iu(r,R,e),e},G=function(r){return Vr(r,R)?r[R]:{}},ir=function(r){return Vr(r,R)});var x,R;pn.exports={set:nr,get:G,has:ir,enforce:lu,getterFor:cu}});var hn=i(function(Qv,qn){var ee=p(),vu=g(),su=y(),ar=I(),re=S(),fu=tn().CONFIGURABLE,du=an(),yn=Qr(),pu=yn.enforce,gu=yn.get,gn=String,or=Object.defineProperty,yu=ee("".slice),qu=ee("".replace),hu=ee([].join),mu=re&&!vu(function(){return or(function(){},"length",{value:8}).length!==8}),bu=String(String).split("String"),xu=qn.exports=function(r,e,t){yu(gn(e),0,7)==="Symbol("&&(e="["+qu(gn(e),/^Symbol\(([^)]*)\)/,"$1")+"]"),t&&t.getter&&(e="get "+e),t&&t.setter&&(e="set "+e),(!ar(r,"name")||fu&&r.name!==e)&&(re?or(r,"name",{value:e,configurable:!0}):r.name=e),mu&&t&&ar(t,"arity")&&r.length!==t.arity&&or(r,"length",{value:t.arity});try{t&&ar(t,"constructor")&&t.constructor?re&&or(r,"prototype",{writable:!1}):r.prototype&&(r.prototype=void 0)}catch(a){}var n=pu(r);return ar(n,"source")||(n.source=hu(bu,typeof e=="string"?e:"")),r};Function.prototype.toString=xu(function(){return su(this)&&gu(this).source||du(this)},"toString")});var te=i(function(rs,mn){var Eu=y(),Su=U(),Ou=hn(),Iu=J();mn.exports=function(r,e,t,n){n||(n={});var a=n.enumerable,o=n.name!==void 0?n.name:e;if(Eu(t)&&Ou(t,o,n),n.global)a?r[e]=t:Iu(e,t);else{try{n.unsafe?r[e]&&(a=!0):delete r[e]}catch(l){}a?r[e]=t:Su.f(r,e,{value:t,enumerable:!1,configurable:!n.nonConfigurable,writable:!n.nonWritable})}return r}});var xn=i(function(es,bn){var wu=Math.ceil,Tu=Math.floor;bn.exports=Math.trunc||function(e){var t=+e;return(t>0?Tu:wu)(t)}});var K=i(function(ts,En){var Pu=xn();En.exports=function(r){var e=+r;return e!==e||e===0?0:Pu(e)}});var On=i(function(ns,Sn){var Cu=K(),Ru=Math.max,_u=Math.min;Sn.exports=function(r,e){var t=Cu(r);return t<0?Ru(t+e,0):_u(t,e)}});var ne=i(function(is,In){var ju=K(),Nu=Math.min;In.exports=function(r){return r>0?Nu(ju(r),9007199254740991):0}});var Tn=i(function(as,wn){var Du=ne();wn.exports=function(r){return Du(r.length)}});var Rn=i(function(os,Cn){var Au=L(),Bu=On(),Mu=Tn(),Pn=function(r){return function(e,t,n){var a=Au(e),o=Mu(a),l=Bu(n,o),u;if(r&&t!=t){for(;o>l;)if(u=a[l++],u!=u)return!0}else for(;o>l;l++)if((r||l in a)&&a[l]===t)return r||l||0;return!r&&-1}};Cn.exports={includes:Pn(!0),indexOf:Pn(!1)}});var ae=i(function(us,jn){var $u=p(),ie=I(),Fu=L(),Lu=Rn().indexOf,Uu=tr(),_n=$u([].push);jn.exports=function(r,e){var t=Fu(r),n=0,a=[],o;for(o in t)!ie(Uu,o)&&ie(t,o)&&_n(a,o);for(;e.length>n;)ie(t,o=e[n++])&&(~Lu(a,o)||_n(a,o));return a}});var ur=i(function(ls,Nn){Nn.exports=["constructor","hasOwnProperty","isPrototypeOf","propertyIsEnumerable","toLocaleString","toString","valueOf"]});var An=i(function(Dn){var Gu=ae(),Ku=ur(),ku=Ku.concat("length","prototype");Dn.f=Object.getOwnPropertyNames||function(e){return Gu(e,ku)}});var Mn=i(function(Bn){Bn.f=Object.getOwnPropertySymbols});var Fn=i(function(ss,$n){var Wu=X(),Hu=p(),zu=An(),Yu=Mn(),Xu=w(),Vu=Hu([].concat);$n.exports=Wu("Reflect","ownKeys")||function(e){var t=zu.f(Xu(e)),n=Yu.f;return n?Vu(t,n(e)):t}});var Gn=i(function(fs,Un){var Ln=I(),Ju=Fn(),Zu=Lr(),Qu=U();Un.exports=function(r,e,t){for(var n=Ju(e),a=Qu.f,o=Zu.f,l=0;l<n.length;l++){var u=n[l];!Ln(r,u)&&!(t&&Ln(t,u))&&a(r,u,o(e,u))}}});var kn=i(function(ds,Kn){var rl=g(),el=y(),tl=/#|\.prototype\./,k=function(r,e){var t=il[nl(r)];return t==ol?!0:t==al?!1:el(e)?rl(e):!!e},nl=k.normalize=function(r){return String(r).replace(tl,".").toLowerCase()},il=k.data={},al=k.NATIVE="N",ol=k.POLYFILL="P";Kn.exports=k});var Hn=i(function(ps,Wn){var oe=q(),ul=Lr().f,ll=er(),cl=te(),vl=J(),sl=Gn(),fl=kn();Wn.exports=function(r,e){var t=r.target,n=r.global,a=r.stat,o,l,u,s,c,v;if(n?l=oe:a?l=oe[t]||vl(t,{}):l=(oe[t]||{}).prototype,l)for(u in e){if(c=e[u],r.dontCallGetSet?(v=ul(l,u),s=v&&v.value):s=l[u],o=fl(n?u:t+(a?".":"#")+u,r.forced),!o&&s!==void 0){if(typeof c==typeof s)continue;sl(c,s)}(r.sham||s&&s.sham)&&ll(c,"sham",!0),cl(l,u,c,r)}}});var Xn=i(function(gs,Yn){var dl=N(),pl=dl("toStringTag"),zn={};zn[pl]="z";Yn.exports=String(zn)==="[object z]"});var Jn=i(function(ys,Vn){var gl=Xn(),yl=y(),lr=M(),ql=N(),hl=ql("toStringTag"),ml=Object,bl=lr(function(){return arguments}())=="Arguments",xl=function(r,e){try{return r[e]}catch(t){}};Vn.exports=gl?lr:function(r){var e,t,n;return r===void 0?"Undefined":r===null?"Null":typeof(t=xl(e=ml(r),hl))=="string"?t:bl?lr(e):(n=lr(e))=="Object"&&yl(e.callee)?"Arguments":n}});var cr=i(function(qs,Zn){var El=Jn(),Sl=String;Zn.exports=function(r){if(El(r)==="Symbol")throw TypeError("Cannot convert a Symbol value to a string");return Sl(r)}});var ri=i(function(hs,Qn){"use strict";var Ol=w();Qn.exports=function(){var r=Ol(this),e="";return r.hasIndices&&(e+="d"),r.global&&(e+="g"),r.ignoreCase&&(e+="i"),r.multiline&&(e+="m"),r.dotAll&&(e+="s"),r.unicode&&(e+="u"),r.unicodeSets&&(e+="v"),r.sticky&&(e+="y"),e}});var ti=i(function(ms,ei){var ue=g(),Il=q(),le=Il.RegExp,ce=ue(function(){var r=le("a","y");return r.lastIndex=2,r.exec("abcd")!=null}),wl=ce||ue(function(){return!le("a","y").sticky}),Tl=ce||ue(function(){var r=le("^r","gy");return r.lastIndex=2,r.exec("str")!=null});ei.exports={BROKEN_CARET:Tl,MISSED_STICKY:wl,UNSUPPORTED_Y:ce}});var ii=i(function(bs,ni){var Pl=ae(),Cl=ur();ni.exports=Object.keys||function(e){return Pl(e,Cl)}});var oi=i(function(ai){var Rl=S(),_l=Ur(),jl=U(),Nl=w(),Dl=L(),Al=ii();ai.f=Rl&&!_l?Object.defineProperties:function(e,t){Nl(e);for(var n=Dl(t),a=Al(t),o=a.length,l=0,u;o>l;)jl.f(e,u=a[l++],n[u]);return e}});var li=i(function(Es,ui){var Bl=X();ui.exports=Bl("document","documentElement")});var yi=i(function(Ss,gi){var Ml=w(),$l=oi(),ci=ur(),Fl=tr(),Ll=li(),Ul=$r(),Gl=Xr(),vi=">",si="<",se="prototype",fe="script",di=Gl("IE_PROTO"),ve=function(){},pi=function(r){return si+fe+vi+r+si+"/"+fe+vi},fi=function(r){r.write(pi("")),r.close();var e=r.parentWindow.Object;return r=null,e},Kl=function(){var r=Ul("iframe"),e="java"+fe+":",t;return r.style.display="none",Ll.appendChild(r),r.src=String(e),t=r.contentWindow.document,t.open(),t.write(pi("document.F=Object")),t.close(),t.F},vr,sr=function(){try{vr=new ActiveXObject("htmlfile")}catch(e){}sr=typeof document!="undefined"?document.domain&&vr?fi(vr):Kl():fi(vr);for(var r=ci.length;r--;)delete sr[se][ci[r]];return sr()};Fl[di]=!0;gi.exports=Object.create||function(e,t){var n;return e!==null?(ve[se]=Ml(e),n=new ve,ve[se]=null,n[di]=e):n=sr(),t===void 0?n:$l.f(n,t)}});var hi=i(function(Os,qi){var kl=g(),Wl=q(),Hl=Wl.RegExp;qi.exports=kl(function(){var r=Hl(".","s");return!(r.dotAll&&r.exec("\n")&&r.flags==="s")})});var bi=i(function(Is,mi){var zl=g(),Yl=q(),Xl=Yl.RegExp;mi.exports=zl(function(){var r=Xl("(?<a>b)","g");return r.exec("b").groups.a!=="b"||"b".replace(r,"$<a>c")!=="bc"})});var pr=i(function(ws,Ei){"use strict";var D=C(),dr=p(),Vl=cr(),Jl=ri(),Zl=ti(),Ql=Q(),rc=yi(),ec=Qr().get,tc=hi(),nc=bi(),ic=Ql("native-string-replace",String.prototype.replace),fr=RegExp.prototype.exec,pe=fr,ac=dr("".charAt),oc=dr("".indexOf),uc=dr("".replace),de=dr("".slice),ge=function(){var r=/a/,e=/b*/g;return D(fr,r,"a"),D(fr,e,"a"),r.lastIndex!==0||e.lastIndex!==0}(),xi=Zl.BROKEN_CARET,ye=/()??/.exec("")[1]!==void 0,lc=ge||ye||xi||tc||nc;lc&&(pe=function(e){var t=this,n=ec(t),a=Vl(e),o=n.raw,l,u,s,c,v,h,d;if(o)return o.lastIndex=t.lastIndex,l=D(pe,o,a),t.lastIndex=o.lastIndex,l;var f=n.groups,T=xi&&t.sticky,m=D(Jl,t),E=t.source,P=0,O=a;if(T&&(m=uc(m,"y",""),oc(m,"g")===-1&&(m+="g"),O=de(a,t.lastIndex),t.lastIndex>0&&(!t.multiline||t.multiline&&ac(a,t.lastIndex-1)!=="\n")&&(E="(?: "+E+")",O=" "+O,P++),u=new RegExp("^(?:"+E+")",m)),ye&&(u=new RegExp("^"+E+"$(?!\\s)",m)),ge&&(s=t.lastIndex),c=D(fr,T?u:t,O),T?c?(c.input=de(c.input,P),c[0]=de(c[0],P),c.index=t.lastIndex,t.lastIndex+=c[0].length):t.lastIndex=0:ge&&c&&(t.lastIndex=t.global?c.index+c[0].length:s),ye&&c&&c.length>1&&D(ic,c[0],u,function(){for(v=1;v<arguments.length-2;v++)arguments[v]===void 0&&(c[v]=void 0)}),c&&f)for(c.groups=h=rc(null),v=0;v<f.length;v++)d=f[v],h[d[0]]=c[d[1]];return c});Ei.exports=pe});var qe=i(function(){"use strict";var cc=Hn(),Si=pr();cc({target:"RegExp",proto:!0,forced:/./.exec!==Si},{exec:Si})});var Pi=i(function(Cs,Ti){var vc=H(),wi=Function.prototype,Oi=wi.apply,Ii=wi.call;Ti.exports=typeof Reflect=="object"&&Reflect.apply||(vc?Ii.bind(Oi):function(){return Ii.apply(Oi,arguments)})});var Ri=i(function(Rs,Ci){var sc=M(),fc=p();Ci.exports=function(r){if(sc(r)==="Function")return fc(r)}});var Bi=i(function(_s,Ai){"use strict";qe();var _i=Ri(),ji=te(),dc=pr(),Ni=g(),Di=N(),pc=er(),gc=Di("species"),he=RegExp.prototype;Ai.exports=function(r,e,t,n){var a=Di(r),o=!Ni(function(){var c={};return c[a]=function(){return 7},""[r](c)!=7}),l=o&&!Ni(function(){var c=!1,v=/a/;return r==="split"&&(v={},v.constructor={},v.constructor[gc]=function(){return v},v.flags="",v[a]=/./[a]),v.exec=function(){return c=!0,null},v[a](""),!c});if(!o||!l||t){var u=_i(/./[a]),s=e(a,""[r],function(c,v,h,d,f){var T=_i(c),m=v.exec;return m===dc||m===he.exec?o&&!f?{done:!0,value:u(v,h,d)}:{done:!0,value:T(h,v,d)}:{done:!1}});ji(String.prototype,r,s[0]),ji(he,a,s[1])}n&&pc(he[a],"sham",!0)}});var Li=i(function(js,Fi){var me=p(),yc=K(),qc=cr(),hc=F(),mc=me("".charAt),Mi=me("".charCodeAt),bc=me("".slice),$i=function(r){return function(e,t){var n=qc(hc(e)),a=yc(t),o=n.length,l,u;return a<0||a>=o?r?"":void 0:(l=Mi(n,a),l<55296||l>56319||a+1===o||(u=Mi(n,a+1))<56320||u>57343?r?mc(n,a):l:r?bc(n,a,a+2):(l-55296<<10)+(u-56320)+65536)}};Fi.exports={codeAt:$i(!1),charAt:$i(!0)}});var Gi=i(function(Ns,Ui){"use strict";var xc=Li().charAt;Ui.exports=function(r,e,t){return e+(t?xc(r,e).length:1)}});var ki=i(function(Ds,Ki){var Ee=p(),Ec=Nr(),Sc=Math.floor,be=Ee("".charAt),Oc=Ee("".replace),xe=Ee("".slice),Ic=/\$([$&'`]|\d{1,2}|<[^>]*>)/g,wc=/\$([$&'`]|\d{1,2})/g;Ki.exports=function(r,e,t,n,a,o){var l=t+r.length,u=n.length,s=wc;return a!==void 0&&(a=Ec(a),s=Ic),Oc(o,s,function(c,v){var h;switch(be(v,0)){case"$":return"$";case"&":return r;case"`":return xe(e,0,t);case"'":return xe(e,l);case"<":h=a[xe(v,1,-1)];break;default:var d=+v;if(d===0)return c;if(d>u){var f=Sc(d/10);return f===0?c:f<=u?n[f-1]===void 0?be(v,1):n[f-1]+be(v,1):c}h=n[d-1]}return h===void 0?"":h})}});var zi=i(function(As,Hi){var Wi=C(),Tc=w(),Pc=y(),Cc=M(),Rc=pr(),_c=TypeError;Hi.exports=function(r,e){var t=r.exec;if(Pc(t)){var n=Wi(t,r,e);return n!==null&&Tc(n),n}if(Cc(r)==="RegExp")return Wi(Rc,r,e);throw _c("RegExp#exec called on incompatible receiver")}});var Bs=la(qe());var jc=Pi(),Yi=C(),gr=p(),Nc=Bi(),Dc=g(),Ac=w(),Bc=y(),Mc=Y(),$c=K(),Fc=ne(),A=cr(),Lc=F(),Uc=Gi(),Gc=Cr(),Kc=ki(),kc=zi(),Wc=N(),Oe=Wc("replace"),Hc=Math.max,zc=Math.min,Yc=gr([].concat),Se=gr([].push),Xi=gr("".indexOf),Vi=gr("".slice),Xc=function(r){return r===void 0?r:String(r)},Vc=function(){return"a".replace(/./,"$0")==="$0"}(),Ji=function(){return/./[Oe]?/./[Oe]("a","$0")==="":!1}(),Jc=!Dc(function(){var r=/./;return r.exec=function(){var e=[];return e.groups={a:"7"},e},"".replace(r,"$<a>")!=="7"});Nc("replace",function(r,e,t){var n=Ji?"$":"$0";return[function(o,l){var u=Lc(this),s=Mc(o)?void 0:Gc(o,Oe);return s?Yi(s,o,u,l):Yi(e,A(u),o,l)},function(a,o){var l=Ac(this),u=A(a);if(typeof o=="string"&&Xi(o,n)===-1&&Xi(o,"$<")===-1){var s=t(e,l,u,o);if(s.done)return s.value}var c=Bc(o);c||(o=A(o));var v=l.global;if(v){var h=l.unicode;l.lastIndex=0}for(var d=[];;){var f=kc(l,u);if(f===null||(Se(d,f),!v))break;var T=A(f[0]);T===""&&(l.lastIndex=Uc(u,Fc(l.lastIndex),h))}for(var m="",E=0,P=0;P<d.length;P++){f=d[P];for(var O=A(f[0]),B=Hc(zc($c(f.index),u.length),0),yr=[],qr=1;qr<f.length;qr++)Se(yr,Xc(f[qr]));var hr=f.groups;if(c){var Pe=Yc([O],yr,B,u);hr!==void 0&&Se(Pe,hr);var Ce=A(jc(o,void 0,Pe))}else Ce=Kc(O,u,B,yr,hr,o);B>=E&&(m+=Vi(u,E,B)+Ce,E=B+O.length)}return m+Vi(u,E)}]},!Jc||!Vc||Ji);var Zi=400;function Ie(r,e){var t=0;if(r.nodeType===3){var n=r.nodeValue.replace(/\n/g,"").length;if(n>=e)return{element:r,offset:e};t+=n}else if(r.nodeType===1&&r.firstChild){var a=Ie(r.firstChild,e);if(a.element!==null)return a;t+=a.offset}return r.nextSibling?Ie(r.nextSibling,e-t):{element:null,offset:t}}function we(r,e,t){for(var n=0,a=0;a<r.childNodes.length;a++){var o=r.childNodes[a];if(o.nodeType===3){for(var l=/\n/g,u=void 0;(u=l.exec(o.nodeValue))!==null;)if(n++,n===e)return Ie(o,u.index+t+1)}else if(o.nodeType===1){var s=we(o,e-n,t);if(s.element!==null)return s;n+=s.offset}}return{element:null,offset:n}}function Zc(r,e){if(!!document.createRange){var t=document.getElementById("srcref_"+r);if(!t){t=document.createElement("span"),t.id="srcref_"+r;var n=r,a=document.getElementById(e.replace(/\./g,"_")+"_code");if(!a)return;var o=we(a,n[0],n[4]),l=we(a,n[2],n[5]);if(o.element===null||l.element===null)return;var u=document.createRange();o.element.parentNode.nodeName==="SPAN"&&o.element!==l.element?u.setStartBefore(o.element.parentNode):u.setStart(o.element,o.offset),l.element.parentNode.nodeName==="SPAN"&&o.element!==l.element?u.setEndAfter(l.element.parentNode):u.setEnd(l.element,l.offset),u.surroundContents(t)}$(t).stop(!0,!0).effect("highlight",null,1600)}}Shiny&&Shiny.addCustomMessageHandler("showcase-src",function(r){r.srcref&&r.srcfile&&Zc(r.srcref,r.srcfile)});var Te=!1,Qi=function(e,t){var n=t?Zi:1,a=e?document.getElementById("showcase-sxs-code"):document.getElementById("showcase-code-inline"),o=e?document.getElementById("showcase-code-inline"):document.getElementById("showcase-sxs-code"),l=document.getElementById("showcase-app-metadata");if(l===null){var u=$("#showcase-well");e?u.fadeOut(n):u.fadeIn(n)}$(a).hide(),$(o).fadeOut(n,function(){var s=document.getElementById("showcase-code-tabs");o.removeChild(s),a.appendChild(s),e?ea():document.getElementById("showcase-code-content").removeAttribute("style"),$(a).fadeIn(n),e||(document.getElementById("showcase-app-container").removeAttribute("style"),t&&$(document.body).animate({scrollTop:$(a).offset().top}));var c=document.getElementById("readme-md");c!==null&&(c.parentElement.removeChild(c),e?(o.appendChild(c),$(o).fadeIn(n)):document.getElementById("showcase-app-metadata").appendChild(c)),document.getElementById("showcase-code-position-toggle").innerHTML=e?'<i class="fa fa-level-down"></i> show below':'<i class="fa fa-level-up"></i> show with app'}),e&&$(document.body).animate({scrollTop:0},n),Te=e,ra(e&&t),$(window).trigger("resize")};function ra(r){var e=960,t=e,n=1,a=document.getElementById("showcase-app-code").offsetWidth;a/2>e?t=a/2:a*.66>e?t=960:(t=a*.66,n=t/e);var o=document.getElementById("showcase-app-container");$(o).animate({width:t+"px",zoom:n*100+"%"},r?Zi:0)}var Qc=function(){Qi(!Te,!0)},rv=function(){document.body.offsetWidth>1350&&Qi(!0,!1)};function ea(){document.getElementById("showcase-code-content").style.height=$(window).height()+"px"}function ev(){var r=document.getElementById("showcase-markdown-content");if(r!==null){var e=r.innerText||r.innerHTML,t=window.Showdown.converter;document.getElementById("readme-md").innerHTML=new t().makeHtml(e)}}$(window).resize(function(){Te&&(ra(!1),ea())});window.toggleCodePosition=Qc;$(window).on("load",rv);$(window).on("load",ev);window.hljs&&window.hljs.initHighlightingOnLoad();})();
//# sourceMappingURL=shiny-showcase.js.map
