This project heavily depends on the functionality provided in
the mrmath library. You can download the libarary from

http://www.mrsoft.org/

or if you like a convenient svn download then subscribe to
http://code.google.com/p/mrmath/

You can add the matrix library to your projects search path for easy 
integration.

##############################################################

Active Appearance Models here are based on the work of Cootes et al
"Active Appearance Models" 
https://www.cs.cmu.edu/~efros/courses/LBMV07/Papers/cootes-eccv-98.pdf

Some hints on using the AAMBuilder project:

* You can annotate images with the annotator tool. Note that it is required to
  always annotate the images in the same order and always use the same number of points!
* To build an Active Appearance Module one needs a directory having 
  a set of images with a corresponding points file (same name as the image
  but with a different extension). The points files may be created by using 
  the AAMAnnotation tool. 
* The predefined values for building an AAM are quite ok - note that the 
  model size (aka the models maximum internal image size) has quite some inpact on
  the performance.
* You can choose between two different image warping techniques: Linear where the
  image is splitted into triangles and these are warped to a different coordinate system or
  Thin Plate Splines where the warping is defined by a global spline.
* The library supports a pyramid approach going from the lowest to the highest resolution.
* You can load previously created models.
* To test the model goto the "Test Model Fitting" tab and load an image. Click on the 
  image somewhere this initializes the fitting algorithm aka the image is placed at the clicked point and
  the models tries to fit the underlying image. Examine the result on the right image (the fitting 
  can take quite some time according to the size of the loaded image, the number of iterations needed and
  the warping used).  