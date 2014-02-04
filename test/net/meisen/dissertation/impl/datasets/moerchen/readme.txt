These interval datasets were first used in
 
Mörchen, F., Fradkin, D.: Robust mining of time intervals with semi-interval partial order patterns, In Proceedings SIAM Conference on Data Mining, (2010), pp. 315-326 (http://www.mybytes.de/papers/moerchen10robust.pdf)

Please see this paper for more details on the dataset generation and quote it when using them. I would also love to know what you are working on in the area of interval mining.

Best
Fabian Moerchen (fabian@mybytes.de)

PS Thanks go to Moloud Shahbazi for helping with this readme.

---

There are 7 different datasets available:

 1. asl-bu
 2. asl-gt-thad
 3. auslan2
 4. blocks
 5. context
 6. pioneer
 7. skating

for each of the first 6 datasets, there exist 4 different files:
   1. "*_intervals.int"
   2. "*_intervals.int.tskm"
   3. "*_windows.int"
   4. "*_windows.int.tskm"

Each files description is as following:
  1. "*_intervals.int" :
    -- All the instances are merged into "*_intervals.int" data file with a simulated continuous time.
    -- each line is a "time interval" data represented by 3 values, <symbol  start_time end_time>.
    -- each line belongs to an instance represented in *_windows.int file.

  2. "*_intervals.int.tskm":
    -- this file describes the labels of each symbol
    -- each line includes 2 values <symbol label>

  3. "*_windows.int" :
    -- "*_windows.int" marks the segments on the complete timeline that correspond to the instances.
    -- each line in this file represents an instance using the time range assigned to that instance.
    -- each line includes 3 values, <class begin end>.
    -- class specifies the class that the instance belongs to.
    -- begin and end defines the assigned timeline for the instance.
    -- the time interval <symbol_i s_i e_i>  in "*_intervals.int" file belongs to the instance in <class_j b_j e_j> in "*_windows.int" file,
      iff s_i>=b_j && e_i<=e_j.

  4. "*_windows.int.tskm" :
    -- this file describes the labels of each class of data.
    -- each line includes 2 values <class label>.

**for the skating dataset, there are two more files which describe which instance of a certain class is related to object.

