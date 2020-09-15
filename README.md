# affinemcmcfit
An IDL implementation of the Goodman &amp; Weare affine invariant MCMC sampler. Designed for some compatibility with functions written for Craig Markwardt's mpfit.  

The main function is in affinemcmcfit. An example for the usage is given in this file, in the "testaffine" procedure. To run this, execute in IDL

.compile affinemcmcfit
testaffine

This will perform a quick MCMC fit to simulated linear data. The file "affinemcmcfit.pro" is sufficient to run the MCMC fitter, but without the additional files in the repository, (fifteenb.pro and counter.pro from John Johnson's IDL library, and sixty.pro from the IDL astronomy library) it will not show an estimate of the time remaining in the fit. This is a very helpful diagnostic when running long MCMC fits that may take days or weeks. 
