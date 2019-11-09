uniform float iOvertoneVolume;
uniform float iGlobalBeatCount;
uniform float iActiveColor;
uniform float iA;
uniform float iB;
uniform float iC;
uniform float outt;
out vec4 out_Color;

vec2 distortUV(vec2 uv, vec2 nUV, sampler2D nstex ,  float ip1)
{
  vec2 uv_orig=uv;
  float intensity = 0.1;
  float scale = 0.05;//*ip1;
  float speed = 1.06;


    nUV.x += ip1*speed;
    nUV.y += ip1*speed ;
    vec2 noise= texture(nstex, nUV*scale).xy;

    uv += ((-1.0+noise*2));// * intensity;

    return mix(uv, uv_orig, 0);;
}

vec2 noiseUV(vec2 uv, float mod1, float mod2){
  vec2 block =floor(gl_FragCoord.xy/vec2(16*1.01*mod2));
  vec2 uv_noise = block / vec2(64);
  uv_noise +=floor(vec2(mod1*0.3) * vec2(12345.0, 3543.0))/vec2(sqrt(mod1));
  return uv_noise;

}

vec4 glitch(vec2 uv_noise, vec2 uv,  vec4 v1In, vec4 v2In, float mod1, sampler2D tex1, sampler2D tex2){
  float block_thres =pow(fract(mod1+12999.0), 2.0)*0.02;
  float line_thres =pow(fract(mod1+ 33336), 3.0)* 0.7;

  vec2 uv_r =uv, uv_g=uv, uv_b=uv;


  if (v1In.r< block_thres ||
      v2In.g <line_thres){
    vec2 dist = (fract(uv_noise)-0.5)*0.3;
    uv_r +=dist*0.1;
    uv_g +=dist*1.2;
    uv_b +=dist*0.125;
      }

  vec4 glitchText=v1In;
	// loose luma for some blocks
	if (texture2D(tex1, uv_noise).g < block_thres)
		glitchText.rgb = v2In.ggg;

        	// discolor block lines
	if (texture2D(tex2, vec2(uv_noise.y, 0.0)).b * 2.5 < line_thres)
          glitchText.rgb = vec3(0.0, dot(glitchText.rgb, vec3(1.0)), 0.0);


	// interleave lines in some blocks
	if (texture2D(tex1, uv_noise).g * 0.05  < block_thres ||
		texture(tex1, vec2(uv_noise.y, 0.0)).g * 2.5 < line_thres) {
		float line = fract(gl_FragCoord.y / 3.0);
		vec3 mask = vec3(3.0, 0.0, 0.0);
		if (line > 0.333){
                  //discard;
                  mask = vec3(100.0, 3.0, 0.0);
                }
		if (line > 0.666)
			mask = vec3(0.0, 0.0, 3.0);

		glitchText.xyz *= mask;
	}

        return glitchText;

}


vec4 colorRemoval(vec4 fg, vec4 bg, float th, float mod1, float r, float g, float b){
  vec3 color_diff = fg.rgb - vec3(r, g, b);
  float squared_distance = dot(color_diff, color_diff);
  if (squared_distance < (mod1*th))
   {
     fg = bg;
   }

  return fg;

}

vec4 waveColors(vec4 v1In, vec2 uv, float mod1, float ns, float w, float lines){
  const float tau = 6.28318530717958647692;
  vec3 wave = vec3(0.0);
  //float width = v0.x*((iDataArray[0]*iDataArray[0]*iDataArray[0]*iDataArray[0])/100000000);
  float n=10;
  float width=w*mod1;
  for (int i=0; i < lines; i++){
    n=1; //sin(iDataArray[0]);
    float sound =v1In.x;
    float xymix=mix(uv.y, uv.x, 0);

    float a = 0.1*float(i)*tau/float(n);
    vec3 phase = smoothstep(-1.0,1.5, vec3(cos(a), cos(a-tau/3.0), cos(a-tau*2.0/3.0)));
    wave += phase * smoothstep(width, 0.0, abs(xymix - ((sound*0.5)+0.2)));

    //This shift of uv.x means our index into the sound data also
    //moves along, examining a different part of the audio wave.
    uv.x += 0.4/float(n);
    uv.y -= 0.05*mod1;
  }
  wave *= 10/float(10); // * iDataArray[0];
  vec4 cf8=vec4(wave, 1);
  return cf8;
}

vec4 chromaKey(vec4 fg, vec4 bg){
  float maxrb = max( fg.r, fg.g);
  float k = clamp( (fg.b-maxrb)*90, 0.0, 1.0);

  float dg = fg.b;
  fg.b = min( fg.b, maxrb);//iDataArray[0]);
    fg += dg - fg.b;

    vec4 cf6=mix(fg, bg, k); //-sin(iDataArray[0]));
    return cf6;
}

vec4 kaleoidscope(vec2 n, float mod1, float numbi, float numbc,  sampler2D tex0)
{
  vec2  u = n;
        vec2 p = -1. + 2. * u;
	float t = mod1,
          a = atan(p.y, p.x) ,
          r = length(p)*1 ,
          c = numbc * cos(t + 7. * a);
        float numb= numbi;
	vec4 o = vec4(
        texture(
            tex0,
            vec2(numb * a / 5.14, -t + sin(numb  * r + t) + c) * 0.5)) * (1.75 + 0.75 * (sin(t + numb * r) + c));
        return o;
}


//Artifact code adapted from https://www.shadertoy.com/view/Md2GDw
void main(void){
  float it0=iDataArray[0];
  float it1=iDataArray[1];
  float it2=iDataArray[2];
  float it3=iDataArray[3];
  float it4=iDataArray[4];
  float it5=iDataArray[5];
  float it6=iDataArray[6];
  float it7=iDataArray[7];

  int textId=int(iDataArray[51]);

  vec2 uv = (gl_FragCoord.xy / iResolution.xy);
  //uv.x=uv.x*it1;
  //uv=gl_FragCoord.xy/vec2(1080,1920);
  vec2 uv_noise=noiseUV(uv, 1, 0.01/(it7 ));

  vec2 uvi=uv;
  uv.y=1.0-uv.y;
  vec2 dsUV=distortUV(uv, uv, iVideo1, 2*it7);

  vec4 t0 = texture2D(iChannel0,uv);

  vec4 fftw=texture2D(iFftWave, uv);

  vec4 pf = texture2D(iPreviousFrame, uvi);

  vec4 pfd = texture2D(iPreviousFrame, dsUV);



  vec4 c0 = texture2D(iCam0,uv);
  vec4 c1 = texture2D(iCam1,uv);
  vec4 c2 = texture2D(iCam2,uv);
  //vec4 c3 = texture2D(iCam3,uv);
  vec2 uvc4 = (gl_FragCoord.xy / iResolution.xy);
  uvc4.y=uvc4.y;
  vec4 c3 = texture2D(iCam3,uvc4);
  vec4 c4 = texture2D(iCam4,uvc4);


  vec4 c0n = texture2D(iCam0,uv_noise);
  vec4 c1n = texture2D(iCam1,uv_noise);
  vec4 c2n = texture2D(iCam2,uv_noise);
  vec4 c3n = texture2D(iCam3,uv_noise);
  vec4 c4n = texture2D(iCam4,uv_noise);


  vec4 c0d = texture2D(iCam0,dsUV);
  vec4 c1d = texture2D(iCam1,dsUV);
  vec4 c2d = texture2D(iCam2,dsUV);
  vec4 c3d = texture2D(iCam3,dsUV);
  vec4 c4d = texture2D(iCam4,dsUV);


  vec2 uv_comp=uv;
  uv_comp.x=uv_comp.x*0.5+uv_comp.x*0.5*1;

  vec2 uv_comp2=uv;
  uv_comp2.x=uv_comp2.x*0.75;

  vec4 v0= texture2D(iVideo0, uv_comp);
  vec4 v1= texture2D(iVideo1, uv);
  vec4 v2= texture2D(iVideo2, uv_comp2);
  vec4 v3= texture2D(iVideo3, uv_comp);
  vec4 v4= texture2D(iVideo4, uv);


  vec4 v0n= texture2D(iVideo0, uv_noise);
  vec4 v1n= texture2D(iVideo1, uv_noise);
  vec4 v2n= texture2D(iVideo2, uv_noise);
  vec4 v3n= texture2D(iVideo3, uv_noise);
  vec4 v4n= texture2D(iVideo4, uv_noise);

  vec4 v0d= texture2D(iVideo0, dsUV);
  vec4 v1d= texture2D(iVideo1, dsUV);
  vec4 v2d= texture2D(iVideo2, dsUV);
  vec4 v3d= texture2D(iVideo3, dsUV);
  vec4 v4d= texture2D(iVideo4, dsUV);

  vec4 v0kal= kaleoidscope(uv, it5 , 5, it4*it5*0.001, iVideo4);

  vec4 text= texture2D(iText, uv);

  c3.rgb=c3.bgr;
  vec4 mask =vec4(0,1,0,0);
  if (gl_FragCoord.x>1*2000){
     c3=mask;
   }

  vec4 vm=mix(v3, v2, 0);

  vec4 vt1=colorRemoval(vm, v0, 0.5, 1, 0,0.0,0.9);

  vec4 vi=mix(c3, v3, 0);

  vec4 vt3=colorRemoval(vi, v0kal, 0.2, 1, 0.1,0.0,0.6);

  vec4 vt4=colorRemoval(v0, v0kal, 0.6, 1 , 0, 0, 0);

  vec4 vmss=mix(v2d, v0, 10);

  out_Color=mix(vmss, pfd, 0.89*it7*it7*10); //vt4;

}
