# The Persistence and Effects of Exploding Offers in Job Matching Programs

A practical model for understanding the welfare effects of exploding offers in job markets

The code here simulates a job market to understand the existence and welfare effects of exploding offers (offers that job candidates must either accept or forfeit).

The simulation uses the following general procedure:
- Simulate employee preferences over employers from a Beta distribution
- Simulate employer preferences over employees
- Run a deferred acceptance algorithm, finding the a stable matching based on reported preferences
- Check any matching for blocking pairs
- Allow for employer preferences to depend on employee enthusiasm (interdependence of employee/employer preferences)
- Allow employers to extend exploding offers to employees
- Analyze the welfare effects (extent of blocking pairs) before and after exploding offers are allowed
