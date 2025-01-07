## Mandelbrot set

This app was built from [Heroku's scala starter](https://devcenter.heroku.com/articles/getting-started-with-scala).

TO DO:
- Invoke calcIterNo only for hexels that border the boundary.
- Use local storage for successive increases of maxIter.
- Handle Heroku crashes better?
- Use better means (derbailing) of determining whether sequence will diverge?
