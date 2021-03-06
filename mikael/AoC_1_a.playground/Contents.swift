import Foundation
import XCTest

struct Calculator {
    let input: [Int]!
    var isHalf = false
    init(input:[Int], isHalf: Bool) {
        self.input = input
        self.isHalf = isHalf
    }
    
    func calculate() -> Int {
        var prev = input.first!
        return input.enumerated().map { (index, element) -> Int in

            if index == 0 && input.first! == input.last! {
                return input.first!
            }
            
            if prev == element {
                if index == 0 { return 0 }
                return element
            }
            prev = element
            return 0
        }.reduce(0, {$0 + $1})
    }
    
    func calculate2() -> Int {
        let offset = input.count / 2
        var prev = input.first!
        return input.enumerated().map { (index, element) -> Int in
            
            var indexWithOffset = index + offset
            if indexWithOffset == input.count { indexWithOffset = 0 }
            print(input[indexWithOffset])
            
            if index == 0 && input.first! == input.last! {
                return input.first!
            }
            
            if prev == input[indexWithOffset] {
                if index == 0 { return 0 }
                return input[indexWithOffset]
            }
            prev = input[indexWithOffset]
            return 0
            }.reduce(0, {$0 + $1})
    }
}

struct Input {
    func inputData() -> String {
        return "516299281491169512719425276194596424291268712697155863651846937925928456958813624428156218468331423858422613471962165756423837756856519754524985759763747559711257977361228357678293572698839754444752898835313399815748562519958329927911861654784216355489319995566297499836295985943899373615223375271231128914745273184498915241488393761676799914385265459983923743146555465177886491979962465918888396664233693243983969412682561799628789569294374554575677368219724142536789649121758582991345537639888858113763738518511184439854223386868764189133964543721941169786274781775658991329331759679943342217578532643519615296424396487669451453728113114748217177826874953466435436129165295379157226345786756899935747336785161745487933721527239394118721517195849186676814232887413175587327214144876898248571248517121796766248817366614333915154796983612174281237846165129114988453188844745119798643314857871527757831265298846833327863781341559381238458322786192379487455671563757123534253463563421716138641611915686247343417126655317378639314168461345613427262786624689498485599942336813995725145169355942616672812792174556866436158375938988738721253664772584577384558696477546232189312287262439452141564522329987139692281984783513691857538335537553448919819545332125483128878925492334361562192621672993868479566688564752226111784486619789588318171745995253645886833872665447241245329935643883892447524286642296955354249478815116517315832179925494818748478164317669471654464867111924676961162162841232473474394739793968624974397916495667233337397241933765513777241916359166994384923869741468174653353541147616645393917694581811193977311981752554551499629219873391493426883886536219455848354426461562995284162323961773644581815633779762634745339565196798724847722781666948626231631632144371873154872575615636322965353254642186897127423352618879431499138418872356116624818733232445649188793318829748789349813295218673497291134164395739665667255443366383299669973689528188264386373591424149784473698487315316676637165317972648916116755224598519934598889627918883283534261513179931798591959489372165295"
    }
    
    func intData() -> [Int] {
        let charArray = Array(inputData())
        let intArray = charArray.map { String($0) }.map { Int($0)! }
        return intArray
    }
}

struct Day1 {
    
    func problem1() -> Int {
        let input = Input()
        let calculator = Calculator(input: input.intData(), isHalf: false)
        return calculator.calculate()
    }
    
    func problem2() -> Int {
        let input = Input()
        let calculator = Calculator(input: input.intData(), isHalf: false)
        return calculator.calculate2()
    }
}


class MyTests:XCTestCase {
//    func testExample1() {
//        let sut = Calculator(input: [1,1,2,2], isHalf: false)
//        let expected = 3
//
//        let result = sut.calculate()
//        XCTAssertEqual(result, expected)
//    }
//
//    func testExample2() {
//        let sut = Calculator(input: [1,1,1,1], isHalf: false)
//        let expected = 4
//
//        let result = sut.calculate()
//        XCTAssertEqual(result, expected)
//    }
//
//    func testExample3() {
//        let sut = Calculator(input: [1,2,3,4], isHalf: false)
//        let expected = 0
//
//        let result = sut.calculate()
//        XCTAssertEqual(result, expected)
//    }
//
//    func testExample4() {
//        let sut = Calculator(input: [9,1,2,1,2,1,2,9], isHalf: false)
//        let expected = 9
//
//        let result = sut.calculate()
//        XCTAssertEqual(result, expected)
//    }
    
//    func testExample1b() {
//        let sut = Calculator(input: [1,2,1,2], isHalf: false)
//        let expected = 6
//
//        let result = sut.calculate2()
//        XCTAssertEqual(result, expected)
//    }

//    func testExample2b() {
//        let sut = Calculator(input: [1,2,2,1], isHalf: false)
//        let expected = 0
//
//        let result = sut.calculate()
//        XCTAssertEqual(result, expected)
//    }
//
//    func testExample3b() {
//        let sut = Calculator(input: [1,2,3,4,2,5], isHalf: false)
//        let expected = 4
//
//        let result = sut.calculate2()
//        XCTAssertEqual(result, expected)
//    }
//
    func testExample4b() {
        let sut = Calculator(input: [1,2,3,1,2,3], isHalf: false)
        let expected = 12

        let result = sut.calculate2()
        XCTAssertEqual(result, expected)
    }
//    func testExample5b() {
//        let sut = Calculator(input: [1,2,1,3,1,4,1,5], isHalf: false)
//        let expected = 9
//
//        let result = sut.calculate()
//        XCTAssertEqual(result, expected)
//    }
}

MyTests.defaultTestSuite.run()


//let solution = Day1().problem2()
//print(solution)

