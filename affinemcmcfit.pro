; The default is for affinemcmcfit to expect negative log likelihoods. Throwing a /chisq lets it accept chi squared. Throwing a /mpfunction lets it accept
; deviates e.g. from functions written for mpfit.

; Written by Andrew Vanderburg, and including functions written by Jason Wright and John Johnson. 

pro testaffine
  true_slope = 1.5d
  true_intercept = 4d
  error = 1d
  extra = {x:linspace(0, 10, 30), y:true_intercept + true_slope* linspace(0, 10, 30) + error * jrandomn(seed, 30), e: fltarr(30) + error}
  p = affinemcmcfit('linmcmcfit', [0,0], functargs = extra, nlink=1000, perror=perror, chains = chains, whichwalker = whichwalker, whichlink = whichlink, nburn = 500,/quiet)
  print, 'Slope: ' + sigfig(p[0],.1) + ' +/- ' + sigfig(perror[0],.1) + ', compared to the true value of ' + trim(true_slope)
  print, 'Intercept: ' + sigfig(p[1],.1) + ' +/- ' + sigfig(perror[1],.1) + ', compared to the true value of ' + trim(true_intercept)
  stop
end

function linspace, a, b, n
  l = dindgen(n) / (n-1.0d) * (b - a) + a
  return, l
end


function linmcmcfit, p, x = x, y = y, e = e
  return, ((y - (p[0] * x + p[1]))^2 / (2*e^2))
end

function polymcmcfit, p, x = x, y = y, e = e
  model = dblarr(n_elements(x))
  for i = 0, n_elements(p)-1 do begin
    model += p[i] * x^i
  endfor
  return, ((y - model)^2 / (2*e^2))
end

function jrandomn, seed, d1, d2, d3, d4, d5, d6, d7, d8, use_seed = use, _extra = extra

  ;Fixes a bug in randomn:  In that routine, if seed is not specified,
  ;a new one is generated.  Unfortunately, this new one is not generated randomly, but based on an internal sequence.  This means that if you rely on this new seed to be random (as most users do), that sequential calls to your routine that calls randomn will have very similar outputs:
  ;For instance, try the following code:
  ;
  ; delvar, seed, seed2
  ; print, randomn(seed,2)
  ; print, randomn(seed2,2)
  ; print, randomn(seed,2)
  ;
  ; the undefined variable "seed2" has not been given a random seed -- it has the next value of "seed"

  ;
  ; jrandom[u|n] addresses this by ensuring that:
  ;   1) sequential calls with undefined seeds to jrandom ALWAYS use a new seed by keeping track of the current seed with a global variable "!random_seed"
  ;   2) ignoring input values of the seed (which may be "stale" values already used by some other call to random) unless explicitly told otherwise.
  ;
  ; Usage:  IDENTICAL to randomn, except:

  ;IF YOU WISH TO SPECIFY THE SEED EXPLICITLY, YOU MUST MUST MUST SET USE_SEED=1

  ;Jason Wright
  ;19 Jan 2009 (less than 24 hours now...)

  defsysv, '!random_seed', exists = x   ;does the global exist?

  if keyword_set(use) and n_elements(seed) ne 0 then sx = 1 else sx = 0;default to get new seeds

  if ~sx then begin  ;no existing seed -- get new one
    if x then seed = !random_seed else begin ;if present, use the global value
      r = randomn(blah, 1)               ;otherwise get a new seed
      seed = blah
    endelse
  endif

  if ~x then begin                      ;if not
    defsysv, '!random_seed', seed    ;make a global
  endif

  case n_params()-1 of                  ;call randomn.pro
    0:ans = randomn(seed, _extra = extra)
    1:ans = randomn(seed, d1, _extra = extra)
    2:ans = randomn(seed, d1, d2, _extra = extra)
    3:ans = randomn(seed, d1, d2, d3, _extra = extra)
    4:ans = randomn(seed, d1, d2, d3, d4, _extra = extra)
    5:ans = randomn(seed, d1, d2, d3, d4, d5, _extra = extra)
    6:ans = randomn(seed, d1, d2, d3, d4, d5, d6, _extra = extra)
    7:ans = randomn(seed, d1, d2, d3, d4, d5, d6, d7, _extra = extra)
    else: ans = randomn(seed, d1, d2, d3, d4, d5, d6, d7, d8, _extra = extra)
  endcase
  !random_seed = seed                   ;update the global
  return, ans
end

function jrandomu, seed, d1, d2, d3, d4, d5, d6, d7, d8, use_seed = use, _extra = extra

  ;Fixes a bug in randomu:  In that routine, if seed is not specified,
  ;a new one is generated.  Unfortunately, this new one is not generated randomly, but based on an internal sequence.  This means that if you rely on this new seed to be random (as most users do), that sequential calls to your routine that calls randomu will have very similar outputs:
  ;For instance, try the following code:
  ;
  ; delvar, seed, seed2
  ; print, randomu(seed,2)
  ; print, randomu(seed2,2)
  ; print, randomu(seed,2)
  ;
  ; the undefined variable "seed2" has not been given a random seed -- it has the next value of "seed"

  ;
  ; jrandom[u|n] addresses this by ensuring that:
  ;   1) sequential calls with undefined seeds to jrandom ALWAYS use a new seed by keeping track of the current seed with a global variable "!random_seed"
  ;   2) ignoring input values of the seed (which may be "stale" values already used by some other call to random) unless explicitly told otherwise.
  ;
  ; Usage:  IDENTICAL to randomu, except:

  ;IF YOU WISH TO SPECIFY THE SEED EXPLICITLY, YOU MUST MUST MUST SET USE_SEED=1

  ;Jason Wright
  ;19 Jan 2009 (less than 24 hours now...)

  defsysv, '!random_seed', exists = x   ;does the global exist?

  if keyword_set(use) and n_elements(seed) ne 0 then sx = 1 else sx = 0;default to get new seeds

  if ~sx then begin  ;no existing seed -- get new one
    if x then seed = !random_seed else begin ;if present, use the global value
      r = randomu(blah, 1)               ;otherwise get a new seed
      seed = blah
    endelse
  endif

  if ~x then begin                      ;if not
    defsysv, '!random_seed', seed    ;make a global
  endif

  case n_params()-1 of                  ;call randomu.pro
    0:ans = randomu(seed, _extra = extra)
    1:ans = randomu(seed, d1, _extra = extra)
    2:ans = randomu(seed, d1, d2, _extra = extra)
    3:ans = randomu(seed, d1, d2, d3, _extra = extra)
    4:ans = randomu(seed, d1, d2, d3, d4, _extra = extra)
    5:ans = randomu(seed, d1, d2, d3, d4, d5, _extra = extra)
    6:ans = randomu(seed, d1, d2, d3, d4, d5, d6, _extra = extra)
    7:ans = randomu(seed, d1, d2, d3, d4, d5, d6, d7, _extra = extra)
    else: ans = randomu(seed, d1, d2, d3, d4, d5, d6, d7, d8, _extra = extra)
  endcase
  !random_seed = seed                   ;update the global
  return, ans
end

function randomaffine, seed, a, d1, d2, d3, d4, d5, d6, d7, d8, _extra = extra


  case n_params()-1 of                  ;call randomn.pro
    1:ans = jrandomu(seed, _extra = extra)
    2:ans = jrandomu(seed, d1, _extra = extra)
    3:ans = jrandomu(seed, d1, d2, _extra = extra)
    4:ans = jrandomu(seed, d1, d2, d3, _extra = extra)
    5:ans = jrandomu(seed, d1, d2, d3, d4, _extra = extra)
    6:ans = jrandomu(seed, d1, d2, d3, d4, d5, _extra = extra)
    7:ans = jrandomu(seed, d1, d2, d3, d4, d5, d6, _extra = extra)
    8:ans = jrandomu(seed, d1, d2, d3, d4, d5, d6, d7, _extra = extra)
    else: ans = jrandomu(seed, d1, d2, d3, d4, d5, d6, d7, d8, _extra = extra)
  endcase

  newans = (ans * (sqrt(a) - sqrt(1./a)) + sqrt(1./a))^2

  return, newans

end

;+
; NAME:
;           FAN
;
; PURPOSE:
;           Take the outer product of the input ARRAY and a
;           UNIT_VECTOR to "fan out" a 1D vector into an array
;           comprised of the vector repeated row-wise NFAN times.
;           Useful for array-wise mathematics (Look Ma, no FOR loops!)
;
; CALLING SEQUENCE:
;           result = fan(array [,nfan, /transpose])
;
; INPUTS:
;           ARRAY - 1D array, input vector
;           NFAN  - number of times to repeat the input vector,
;                   default is N_ELEMENTS(ARRAY)
;
; KEYWORD PARAMETERS:
;
;           TRANSPOSE - Repeat the input vector column-wise
;
; OUTPUTS:
;           A 2D array with N_ELEMENTS(ARRAY) columns and NFAN
;           rows.
;
; EXAMPLE:
;           Fan a FINDGEN of 3 elements, twice.
;
;           IDL> a = findgen(3)
;           IDL> print,fan(a,2)
;                 0.00000      1.00000      2.00000
;                 0.00000      1.00000      2.00000
;
; MODIFICATION HISTORY:
;           Created sometime in ought-2 by JohnJohn
; 06 Dec 2002 JohnJohn- Added some error handling at the beginning
;-
function fan,array,nfan,transpose=transpose
  on_error,2  ;if broke then return to sender
  if n_params() lt 1 then begin
    message,'Syntax: f = fan(array [,nfan, /transpose])',/info
    return,-1
  endif

  if n_elements(nfan) eq 0 then nfan = n_elements(array)
  unit_vector = replicate(1d,nfan)   ;dblarr(nfan)+1.
  if keyword_set(transpose) then new = array##unit_vector $
  else new = unit_vector##array
  return,new
end


function avrandint, seed, n, d1, d2, d3, d4, d5, d6, d7, d8, _extra = extra
  ; returns random integers between 0 and n-1 inclusive.

  case n_params()-1 of                  ;call randomn.pro
    1:ans = jrandomu(seed, _extra = extra)
    2:ans = jrandomu(seed, d1, _extra = extra)
    3:ans = jrandomu(seed, d1, d2, _extra = extra)
    4:ans = jrandomu(seed, d1, d2, d3, _extra = extra)
    5:ans = jrandomu(seed, d1, d2, d3, d4, _extra = extra)
    6:ans = jrandomu(seed, d1, d2, d3, d4, d5, _extra = extra)
    7:ans = jrandomu(seed, d1, d2, d3, d4, d5, d6, _extra = extra)
    8:ans = jrandomu(seed, d1, d2, d3, d4, d5, d6, d7, _extra = extra)
    else: ans = jrandomu(seed, d1, d2, d3, d4, d5, d6, d7, d8, _extra = extra)
  endcase

  return, floor(n * ans)

end




function affinemcmcfit, fcn, start_params_in, widths = width_in, functargs=fcnargs, perror=perror, nwalkers = nwalkers, $
  nlink = nlink, nburnin=nburnin, a_param = a_param, chains = chains, chisq = chisq, frozen = frozen, allneglogl = allneglogl, $
  parinfo = parinfo, cooldown = cooldown, whichwalker = whichwalker, quiet = quiet, verbose = verbose, walkers = walkers, $
  walkerpos = walkerpos, mpfunction = mpfunction, whichlink = whichlink

  start_params = start_params_in

  if keyword_set(verbose) then print, 'Setting up the sampler properties.'

  if 1 - keyword_set(nwalkers) then nwalkers = 200
  if 1 - keyword_set(nlink) then nlink = 1200
  if n_elements(nburnin) eq 0 then begin
    if nlink gt 400 then nburnin = 200
    if nlink le 400 then nburnin = floor(0.5 * nlink)
  end
  if nburnin ge nlink then nburnin = floor(nlink)/2

  npar = n_elements(start_params)



  onedimension = 0
  if npar eq 1 then begin
    onedimension = 1
    npar = 2
    start_params = [start_params[0],0d]
  endif

  if 1 - keyword_set(a_param) then a_param = 2.


  if keyword_set(verbose) then print, 'Setting up limits and frozen parameters.'

  if keyword_set(width_in) then width = width_in

  if 1 - keyword_set(width_in) then width = start_params / 10. + (start_params eq 0)* .1; if there's no better guess for the initial widths of paramters...



  if keyword_set(frozen) then if (where(frozen))[0] gt -1 then width(where(frozen)) = 0d
  lowerlimit = -1/fltarr(npar)
  upperlimit = 1/fltarr(npar)

  if onedimension then begin; Kludge to make one paramter fits work. But really, if you're only fitting one paramter, not really worth MCMC.
    if n_elements(width) eq 1 then width = [width[0],0d]
    width[1] = 0d
  endif

  if keyword_set(parinfo) then begin

    tags = tag_names(parinfo[0])

    if total((strlowcase(tags) eq 'fixed')) gt -1 then if (where(parinfo.fixed))[0] gt -1 then width(where(parinfo.fixed)) = 0d

    if total((strlowcase(tags) eq 'limited')) gt-1 then begin
      for i = 0, npar-1 do begin
        if total(parinfo[i].limited[0]) then lowerlimit[i] = parinfo[i].limits[0]
        if total(parinfo[i].limited[1]) then upperlimit[i] = parinfo[i].limits[1]
      end
    end
  end

  if keyword_set(verbose) then print, 'Initializing walkers.'
  walker = {position:dblarr(nlink, npar), thispos:dblarr(npar), lastneglogl:1./0., allneglogl:dblarr(nlink), whichwalker:intarr(nlink), whichlink:indgen(nlink)}
  walker.allneglogl[0] = walker.lastneglogl
  walkers = replicate(walker, nwalkers)


  walkers.position[0,*] = reform(jrandomn(seed, npar,nwalkers)* fan(width, nwalkers) + fan(start_params,nwalkers), [1,npar,nwalkers]) ;  ; start position for walkers

  walkers.thispos = reform(walkers.position[0,*])

  if keyword_set(verbose) then print, 'Finding the first sample within the allowed parameter ranges.'
  if keyword_set(verbose) then print, 'If the program hangs here, check that your widths are compatible with your parameter ranges.'
  niter = 0l
  if 1 - keyword_set(walkerpos) then begin ; Make sure all of the walkers start out in allowed positions, and calculate - log likelihood for all of them.
    for n = 0, nwalkers-1 do begin
      outofrange = total((walkers[n].thispos lt lowerlimit) or (walkers[n].thispos gt upperlimit)) gt 0
      if outofrange then walkers[n].lastneglogl = !values.D_INFINITY
      if 1 - outofrange then begin

        if 1 - keyword_set(mpfunction) then begin
          walkers[n].lastneglogl = total(call_function(fcn, walkers[n].thispos, _EXTRA=fcnargs))
          if keyword_set(chisq) then walkers[n].lastneglogl *= 0.5 ; log likelihood = -1/2 chi squared
        end;if 1 - keyword_set(mpfunction)

        if keyword_set(mpfunction) then walkers[n].lastneglogl = total(call_function(fcn, walkers[n].thispos, _EXTRA=fcnargs)^2 * 0.5)

      endif
      walkers[n].whichwalker += n
      while 1- finite(walkers[n].lastneglogl) do begin
        walkers[n].position[0,*] = jrandomn(seed, npar) * width + start_params
        walkers[n].thispos = reform(walkers[n].position[0,*])
        outofrange = total((walkers[n].thispos lt lowerlimit) or (walkers[n].thispos gt upperlimit)) gt 0
        if outofrange then thiswalkerlastnll = !values.D_INFINITY
        if 1 - outofrange then begin

          if 1 - keyword_set(mpfunction) then begin
            thiswalkerlastnll = total(call_function(fcn, walkers[n].thispos, _EXTRA=fcnargs))
            if keyword_set(chisq) then thiswalkerlastnll *= 0.5 ; log likelihood = -1/2 chi squared
          end;if 1 - keyword_set(mpfunction)

          if keyword_set(mpfunction) then thiswalkerlastnll = total(call_function(fcn, walkers[n].thispos, _EXTRA=fcnargs)^2 * 0.5)
        endif

        walkers[n].lastneglogl = thiswalkerlastnll
        niter ++
        if niter mod 1d6 eq 0 then print, "Can't find initial starting parameters that are allowed within ranges. " + strtrim(niter,1) + " tries."
      end
    end
  end
  if keyword_set(walkerpos) then begin
    walkers.position[0,*] = walkerpos
    walkers.thispos = reform(walkers.position[0,*])
    for n = 0, nwalkers-1 do begin
      if 1 - keyword_set(mpfunction) then begin
        thiswalkerlastnll = total(call_function(fcn, walkers[n].thispos, _EXTRA=fcnargs))
        if keyword_set(chisq) then thiswalkerlastnll *= 0.5 ; log likelihood = -1/2 chi squared
      end
      if keyword_set(mpfunction) then thiswalkerlastnll = total(call_function(fcn, walkers[n].thispos, _EXTRA=fcnargs)^2 * 0.5)
      walkers[n].lastneglogl =thiswalkerlastnll
      walkers[n].whichwalker += n
    endfor
  endif

  if keyword_set(verbose) then print, 'Starting to run MCMC chains.'
  tstart = systime(/sec)

  nfree = n_elements(where(width ne 0d))
  if keyword_set(verbose) then print, 'There are '+strtrim(nfree,1)+' free parameters.'


  for i = 1l, nlink -1 do begin
    for k = 0l, nwalkers-1 do begin
      ;choose a walker
      j = avrandint(seed, nwalkers-1)
      j += j ge k ; can't be the same walker as we're working on now
      jthpos = reform(walkers[j].position[i-1,*])
      z = randomaffine(seed, a_param); draws a random number from the distribution 1/sqrt(x) from 1/a to a, where a is 2 (second argument).
      newpars = jthpos + z * (walkers[k].thispos - jthpos); update the position

      outofrange = total((newpars lt lowerlimit) or (newpars gt upperlimit)) gt 0
      if outofrange then neglogl = !values.D_INFINITY

      if 1 - outofrange then begin
        if 1 - keyword_set(mpfunction) then begin
          neglogl = total(call_function(fcn, newpars, _EXTRA=fcnargs)); evaluate likelihood of new position
          if keyword_set(chisq) then neglogl *= 0.5 ; log likelihood = -1/2 chi squared
        end;if 1 - keyword_set(mpfunction)
        if keyword_set(mpfunction) then neglogl = total(call_function(fcn, newpars, _EXTRA=fcnargs)^2 * 0.5)
      endif


      q = z^(nfree-1) * exp(walkers[k].lastneglogl - neglogl) ; The dimension of the parameterspace is the number of free parameters.

      r = jrandomu(seed)
      if r le q then begin
        thispars = newpars ; update parameters
      end
      if r gt q then begin
        thispars = walkers[k].thispos; revert to last pars
        neglogl = walkers[k].lastneglogl ; revert to last negative log likelihood
      end

      walkers[k].position[i, *] = thispars
      walkers[k].thispos = thispars
      walkers[k].lastneglogl = neglogl
      walkers[k].allneglogl[i] = neglogl

    end

    if 1 - keyword_set(quiet) then begin
      tremaining = (systime(/sec) - tstart)/double(i) * (nlink - i - 1)
      sexa = sixty(tremaining/3600d)
      ;str = sigfig(sexa(0),2) + ' hours, ' + sigfig(sexa(1),2) + ' minutes, ' + sigfig(sexa(2), 2) + ' seconds remain. Link '
      str = string(sexa(0), format = '(I3)')+':'+string(sexa(1), format = '(I02)')+':'+string(sexa(2), format = '(F04.1)') + ' remains, completed link '
      ;str = strtrim(tremaining/60,1)+ ' minutes remain: Link '
      ;if tremaining/3600 gt 1 then str = strtrim(tremaining/3600,1)+ ' hours remain: Link '
    endif
    
    if keyword_set(cooldown) then wait, 3

    if 1 - keyword_set(quiet) then begin
      counter, i+1,nlink,str
    endif
  end

  chains = dblarr(long((nlink - nburnin)) * long(nwalkers), npar)
  p = fltarr(npar)
  perror = fltarr(npar)
  allneglogl = reform(walkers.allneglogl[nburnin:nlink-1])
  whichwalker = reform(walkers.whichwalker[nburnin:nlink-1])
  whichlink = reform(walkers.whichlink[nburnin:nlink-1])

  for i = 0, npar-1 do begin
    chains[*,i] = reform(walkers.position[nburnin:nlink-1,i], long((nlink - nburnin)) * long(nwalkers))
    p[i] = median(chains[*,i])
    perror[i] = stddev(chains[*,i])
  end

  if onedimension then begin
    p = [p[0]]
    perror = [perror[0]]
    chains = chains[*,0]

  endif

  return, (p)

end