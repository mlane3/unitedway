# The Child Well Being Index System:  CWBI-S
**Client:**  United Way Greater Atlanta
**Developer:**  Emory Continuing Education Team

**Purpose:**  This page is the "start here" document to define what's been done, what needs to be done, and how we are doing it.  It is intended to be read by United Way Atlanta and ECE Team Members, particularly new ones.
To that purpose of bringing new members up to speed there is a 2nd document called [LINK](CONTRIBUTOR.MD) that is a brief listing of software, links, and processes to start contributing to the project.


**This system supports the United Way mission of improving child well-being across greater Atlanta - a mission of the highest importance.  The ECE team brings the belief that modern analytics is a force that can change lives for the better.  Let's see what we can accomplish together!**

Prior to diving into the details, let's synch on the conceptual element of this project. We are in the process of evolving CWBI-S from view only of existing state to interactive modeling to optimize the allocation of services, resources, and funds to improve child's lives.  Conceptually the evolution of this system is analogous to the growth of analytics within IT systems: leveraging passive and historical but behaviorally rich data to drive interactive and optimized modeling to shape the future.


## UWGA Mission, Region Served, and the Role of the CWBI and its Components
United Way Atlanta includes a 13 County area, in this region nearly 500,000 children live in communities with low child well-being.  When children thrive, communities can thrive. By ensuring that every child in our community has the opportunity to reach his or her potential, we are building a strong future for Greater Atlanta.

The first and primary role of the CWBI system is to integrate source data for measures in the three primary pillars of _children, family, and community_.  From the evaluation of data in these pillars, the CWBI is calculated to serve as a "yardstick" for child well-being.  Thus, the CWBI is a set of measures that the community can use collectively to assess how well children, the families that support them and the community that surrounds them, are doing.

  ##CWBI is an average of the 3 Pillars, the 14 Measures roll-up under the 3 Pillars as shown:
  - Child:
      + % Low Weight Births
      + % Students Exceeding 3rd Grade Reading Standards *
      + % Students Exceeding 8th Grade Math Standards
      + High School Graduation Rate *
      + High School, College & Career Readiness Score *
      + % Children without Health  Insurance
      + % Children in Poverty
  - Family:
      + % Families Not Financially Stable
      + % Families with Housing Cost Burden
      + % Births to Mothers without a High School Diploma
  - Community:
      + % Enrolled in Post-Secondary Education
      + % Adults without a High School Diploma
      + % Adults without Health Insurance
      + Unemployment Rate

( * indicates particularly strong drivers of outcomes)


## V1. Requirements for Calculation and Viewing of CWBI, its 3 Pillars, and the 14 individual measures
V1.1  United Way Atlanta requires the calculation of 14 measures (7 in the Child group, 3 in the Family group, 4 in the Community group)
V1.2.  Each of 14 indices need to be viewable at the regional, 13 County level as well as at the individual County, or census tract level (there are 776 Census Tracts in the Greater Atlanta 13 County region)
V1.3.  Each of the 3 Pillars (Child, Family, Community) has an index value calculated using the subset of the 14 indices that pertain to that Pillar (see grouping below).
V1.4.  Each of the 3 Pillar indices can be viewed for the regional, 13 County level as well as at the individual County, or individual census tract level
V1.5.  The overall CWBI is calculated by combining and averaging the 3 Pillar indices
V1.6.  As with the 14 individual indices and the 3 Pillar groups, the overall CWBI must be viewable at the regional, County, and Census Tract level

**Note on measures:**
1. To facilitate comparison across indicators, all indicator values were standardized to z-scores* using the mean and standard deviation across all 776 census tracts.
* zi = (xi-mean)/std deviation, where  xi =indicator value for tract i  |  (Range  of z-scores was -3 to +3).
2. z scores were further transformed so that the magnitude of each z-score is consistent with higher z-values indicating higher levels of opportunity based on that component. For example, for poverty, a high value reflects lower neighborhood opportunity. The z-scores for poverty were multiplied by (-1), so higher poverty rates translated to lower z-scores.
3. f a z-score was outside the -3 to +3 range, it was deemed an outlier. Outlier z-scores were capped at ±3.1

## V1. User Interface (UX) requirements
This section to be added, probably via links to screen shots.  Elements that need to be confirmed or modified based on review between UWGA and ECE include:
**Gauges** relative size, coloring, numeric scale, legend (if applicable), index value (on gauge and whether it should also appear as number in a text box), needle value definition and behavior.
**controls** CWBI-Pillar-Index selection (position on screen, one or multiple list boxes, taskflow), Region-County-CensusTract selection (position on screen, one or multiple list boxes, taskflow)
**sidebar area and gauge area** clarification of sizing, clickable areas, entrance and exit events


## V2. Interactive stage requirements
The above section defines the requirements for calculating and viewing the current state of the CWBI, its three Pillar groups, and the individual index measures.
The CWBI, Pillar, and 14 Measure calculation capabilities were necessary to allow United Way to track progress and **determine what "levers" are most effective** to move toward the big-picture goal: all children are well.

### United Way Great Atlanta Mission and V2 Requirements:
The term "levers" is a fitting descriptor, the objective of this Phase is to allow UWGA to predict **what allocations of resources can have the most positive impact on child well-being into the future**.  ECE is enhancing the system so that UWGA will be able to provide inputs in the form of priorities given to certain measures (akin to real world allocation of resources/funds to certain programs) and then see the impact on the CWBI and other measures.

### Analysis of the Components of Optimization Model
    This is a placeholder to provide "functional architecture" of the optimization model that we are using on this project.  It will help to baseline knowledge across the team, and to avoid confusion by using like terms.

### Requirements for V2 (Interactive) phase of project (3 releases defined):

    2.1 V2, Release 1
    2.1.1 UWGA also has the need to allow a user to **select new specific values for several of the 14 indicators, and forecast the values of the remaining indicators** in order to reach a specified value of CWBI.
    2.1.2 This requirement implies the use of a Optimisation and Equation Solving tool which potentially has to address 13 out of the 14 indicators as part of the forecasted scenario.
    2.1.3 14 sliders (one per indicator) will be provided, each set at the current, actual average value of that indicator for Metro Atlanta.
    2.1.4 The operator can then slide all or any of the indicators all the way to the upper boundary and watch the evolution of the 4 Indexes via a bar graph or other visualization on the same web-page.

    2.2 V2, Release 2
    2.2.1 in version 2 of this stage, the operator will select (direct typing or slider) the desired target value for the Metro Atlanta CWBI.
    2.2.2 The operator will also select new desired values for some of the 14 indicators, and click on a "Execute" button.
    2.2.3 The resulting forecast for the other indicators (not touched by the Operator) will be displayed.

    2.3 V2, Release 3
    2.3 Updating (3/19/18) with most recent info.- this still requires nail-down through client meeting.
        United way wants to do linear optimization at the Zip code level. To do this we will basically do two optimizations. First we the linear optimizer at the county level, which will give us prediction for what the CWBI should be for each county. Then using the predicted final CWBI we solve it at the zip level. **needs feedback from stakeholders**, this involves doing some exploratory data analytics (descriptive analysis) of the results.
        To validate our model for this, we will reach out to experts like Deepa during process.


# CWBI System Architecture
    1. The existing CWBI calculation tool from Excel could not meet the addition of new features, particularly the interactive modeling.  A new technology stack was selected to handle known needs.  Two key capabilities of the environment are support for an interactive, web-based interface as well as robust support for analytics and data visualization.  This includes:
      a. Code written in the R programming language
      b. The RStudio Integrated Development Environment (IDE)
      c. The R Shiny web visualization package (which is an available library within the RStudio IDE)
      d. The Slack collaboration tool, specifically the workspace [link](https://UnitedWayAtlantaEmory.Slack.Com)


##Technical addendum: calculation of measures and indices:
1. To best identify the high need areas in the 13 county region, the CWB Index and the sub-indices were calculated at the census tract level.
2. At this level of geography, observations are missing for some census tracts on some of the indicators. Missing observations were replaced by imputed values generated by the Multiple Imputations procedure using 20 imputations (for a 99.2 % relative efficiency).  This completed dataset was used in all calculations.
3. To facilitate comparison across indicators, all indicator values were standardized to z-scores* using the mean and standard deviation across all 776 census tracts.
4. * zi = (xi-mean)/std deviation, where  xi =indicator value for tract i  |  (Range  of z-scores was -3 to +3).
5. z scores were further transformed so that the magnitude of each z-score is consistent with higher z-values indicating higher levels of opportunity based on that component. For example, for poverty, a high value reflects lower neighborhood opportunity. The z-scores for poverty were multiplied by (-1), so higher poverty rates translated to lower z-scores.
6. If a z-score was outside the -3 to +3 range, it was deemed an outlier. Outlier z-scores were capped at ±3.1


# New Team Members:  How-to stop reading requirements and get started
go to [Link](HowToContribute.MD)

## Licensing and Legal Information - this is a standard section, nothing to scare us here

The proposed applications can be run using free (AGPL v3) Licenses on one or several standalone laptops for UWGA's internal use. Fees will only come into play if UWGA decides to publish the application on a server for wide-spread internal use or for use by the general public.

RStudio is the R Language IDE and is available free of charge under the terms of an AGPL v3 license. This free license is perfectly sufficient for our needs, from a technical standpoint.  A commercial license with professional maintenance (8 Hours Response during Business Hours EST) can be purchased for $995 a year (list prices). (https://www.rstudio.com/products/rstudio/#Desktop)

R Shiny is a web-based User Interface package for RStudio. The R Shiny extension is available as part of the RStudio Package at no additional charge
