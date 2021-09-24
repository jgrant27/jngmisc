package main

import "fmt"

/*

   You're developing a system for scheduling advising meetings with students in a Computer Science program. Each meeting should be scheduled when a student has completed 50% of their academic program.

   Each course at our university has at most one prerequisite that must be taken first. No two courses share a prerequisite. There is only one path through the program.

   Write a function that takes a list of (prerequisite, course) pairs, and returns the name of the course that the student will be taking when they are halfway through their program. (If a track has an even number of courses, and therefore has two "middle" courses, you should return the first one.)

   Sample input 1: (arbitrarily ordered)
   prereqs_courses1 = [
	["Foundations of Computer Science", "Operating Systems"],
	["Data Structures", "Algorithms"],
	["Computer Networks", "Computer Architecture"],
	["Algorithms", "Foundations of Computer Science"],
	["Computer Architecture", "Data Structures"],
	["Software Design", "Computer Networks"]
	]

   In this case, the order of the courses in the program is:
	1st Software Design
	2nd Computer Networks
	Computer Architecture
	Data Structures
	Algorithms
	Foundations of Computer Science
	last Operating Systems

   Sample output 1:
	"Data Structures"


   Sample input 2:
   prereqs_courses2 = [
	["Algorithms", "Foundations of Computer Science"],
	["Data Structures", "Algorithms"],
	["Foundations of Computer Science", "Logic"]
	]


   Sample output 2:
	"Algorithms"

   Sample input 3:
   prereqs_courses3 = [
	["Data Structures", "Algorithms"],
	]


   Sample output 3:
	"Data Structures"

   Complexity analysis variables:

   n: number of pairs in the input

*/
// TODO --- Write your function, returning the result

func main() {
	prereqsCourses1 := [][]string{
		[]string{"Foundations of Computer Science", "Operating Systems"},
		[]string{"Data Structures", "Algorithms"},
		[]string{"Computer Networks", "Computer Architecture"},
		[]string{"Algorithms", "Foundations of Computer Science"},
		[]string{"Computer Architecture", "Data Structures"},
		[]string{"Software Design", "Computer Networks"},
	}

	prereqsCourses2 := [][]string{
		[]string{"Data Structures", "Algorithms"},
		[]string{"Algorithms", "Foundations of Computer Science"},
		[]string{"Foundations of Computer Science", "Logic"},
	}

	prereqsCourses3 := [][]string{
		[]string{"Data Structures", "Algorithms"},
	}

	_, _, _ = prereqsCourses1, prereqsCourses2, prereqsCourses3
	// TODO --- Run the test cases from above through your function, printing the returned results

	fmt.Println(GetNextcourse(prereqsCourses1))
	fmt.Println(GetNextcourse(prereqsCourses2))
	fmt.Println(GetNextcourse(prereqsCourses3))
}

func GetNextcourse(courses [][]string) string {
	// Put the pairs into a map
	m := make(map[string]string)
	for _, p := range courses {
		m[p[0]] = p[1]
	}

	// Create a result array
	a := make([]string, 0)
	for {
		if len(m) == 0 {
			break
		}
		for k, v := range m {
			switch {
			case len(a) == 0:
				a = append(a, k, v)
				delete(m, k)
			case a[len(a)-1] == k:
				a = append(a, v)
				delete(m, k)
			case a[0] == v:
				a = append([]string{k}, a...)
				delete(m, k)
			}
		}
	}

	fmt.Printf("%v %v\n", a, len(a))

	l := len(a) / 2
	if (l > 1 && len(a)%2 == 0) || len(a) == 2 {
		l = l - 1
	}

	return a[l]
}
