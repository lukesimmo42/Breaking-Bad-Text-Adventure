Module Module1
    Const objnum As Integer = 21
    Dim playing As Boolean
    Dim room(12) As String ' the names of the rooms
    Dim desc(12) As String ' the description of the rooms
    Dim exits(12) As String ' possible exits from each room
    Dim objname(objnum) As String
    Dim objname2(objnum) As String
    Dim objlocation(objnum) As Integer
    Dim objdesc(objnum) As String
    Dim objget(objnum) As Boolean
    Dim objgetmessage(objnum) As String
    Dim objpunch(objnum) As String
    Dim objmixed(objnum) As Boolean
    Dim location As Integer ' number representing the current location
    Dim commandWords() As String
    Dim openblinds As Boolean = False
    Dim lighton As Boolean = True
    Dim openbathcab As Boolean = False
    Dim openkitcub As Boolean = False
    Dim locked As Boolean = True
    Dim hosecut As Boolean = False
    Dim pills As Boolean = True
    Dim readytocook As Boolean = False
    Dim cooked As Boolean = False
    Dim lightscrewed = False
    Dim command As String ' current command string
    Sub Main()
        initialise()
        playing = True
        Console.Clear()
        displayRoom()

        While playing
            If location = 3 And objlocation(1) = 0 And lightscrewed = False Then
                Console.WriteLine("You screw the light bulb into the empty socket, it illuiminates the room")
                desc(3) = "The bathtub looks as though it has needed a clean for months, its shower hose lies on the floor. A toothpaste spattered mirror hangs over the sink. You can see a small cabinet next to the door to the south."
                displayRoom()
            End If
            Console.Write("What now?")
            ' get the command from user
            command = Console.ReadLine()
            ' command should be of either a single verb, single direction, or verb noun
            ' parse the command then use verb to identify appropriate action to take
            commandWords = Split(command, " ")
            If UBound(commandWords) >= 0 Then
                ' 'verb' command
                Select Case UCase(commandWords(0))
                    Case "N", "S", "E", "W", "U", "D", "NORTH", "SOUTH", "EAST", "WEST", "UP", "DOWN"
                        If tryMoving(commandWords(0)) Then displayRoom()
                End Select
            End If
            If UBound(commandWords) >= 1 Then
                ' 'verb noun' commands
                Select Case UCase(commandWords(0))
                    Case "ADD"
                        add()
                    Case "COOK"
                        cook()
                    Case "GO", "MOVE"
                        If tryMoving(commandWords(1)) Then displayRoom()
                    Case "EXAMINE", "LOOK", "INSPECT"
                        examine()
                    Case "EAT", "CONSUME", "DRINK"
                        'eat(commandwords(1))
                    Case "PUNCH"
                        punch()
                    Case "GET", "TAKE", "GRAB"
                        take()
                    Case "CUT"
                        cut()
                    Case "UNLOCK"
                        unlock()
                End Select
                If UCase(commandWords(0)) = "OPEN" Then
                    If UCase(commandWords(1)) = "BLINDS" And location = 5 Then
                        If openblinds = False Then
                            openblinds = True
                            Console.WriteLine("You open the blinds")
                            desc(5) = "You are in the bedroom. The light bulb above illuminates the room as well as the window opposite to a large bed."
                            displayRoom()
                            objget(1) = True
                        Else
                            Console.WriteLine("The blinds are already open.")
                        End If
                    End If
                    If UCase(commandWords(1)) = "CABINET" And location = 3 Then

                        If openbathcab = False Then
                            openbathcab = True
                            Console.WriteLine("You open the cabinet, you find empty medicine bottles and a large pack of pills.")
                            objget(8) = True
                            desc(3) = "The bathtub looks as though it has needed a clean for months, its shower hose lies on the floor. A toothpaste spattered mirror hangs over the sink. You can see an open small cabinet next to the door to the south."
                        Else
                            Console.WriteLine("The cabinet is already open.")
                        End If
                    End If
                    If UCase(commandWords(1)) = "CUPBOARD" And location = 7 Then

                        If openkitcub = False And locked = False Then
                            openkitcub = True
                            Console.WriteLine("You open the cupboard under the sink, inside you find a bottle of acid!")
                            objget(17) = True
                        ElseIf openkitcub = False
                            Console.WriteLine("The cupboard is locked.")
                        Else
                            Console.WriteLine("The cupboard is already open.")
                        End If
                    End If

                End If

                If UCase(commandWords(0)) = "CLOSE" And UCase(commandWords(1)) = "BLINDS" And openblinds = True And location = 5 Then
                    Console.WriteLine("You just opened them idiot.")
                End If

                If (UCase(commandWords(0)) = "FLICK" Or UCase(commandWords(0)) = "PRESS" Or UCase(commandWords(0)) = "PUSH") And (UCase(commandWords(1)) = "BUTTON" Or UCase(commandWords(1)) = "SWITCH") Then
                    lightoff()
                End If
            End If
            If UBound(commandWords) >= 2 Then

                If UCase(commandWords(0)) = "TURN" And UCase(commandWords(1)) = "OFF" And (UCase(commandWords(2)) = "LIGHT" Or UCase(commandWords(2)) = "LIGHTBULB") Then
                    lightoff()
                End If
            End If
        End While
    End Sub

    Sub displayRoom()
        Console.Title = room(location)
        Console.WriteLine(desc(location))
        For i = 0 To objnum
            If location = objlocation(i) And objget(i) = True Then
                Console.WriteLine("You can see " & objname2(i) & ".")
            End If
        Next
    End Sub

    Sub add()
        For i = 0 To objnum
            If UCase(objname(i)) = UCase(System.Text.RegularExpressions.Regex.Split(command, commandWords(0) + " ")(1)) Then

                If objlocation(i) = 0 Then

                    If objmixed(i) = False Then
                        objmixed(i) = True
                        readytocook = True
                        Console.WriteLine("You place the " & objname(i) & " in the beaker.")
                    Else
                        Console.WriteLine("")
                    End If
                Else
                    Console.WriteLine("That item is not in your inventory.")
                End If
            End If
        Next
    End Sub

    Sub cook()
        If readytocook = True Then

            If cooked = False Then
                cooked = True
                Console.Write("you place the beaker inside the oven and wait for it to finish")
                If objmixed(0) = False And objmixed(1) = False And objmixed(2) = False And objmixed(3) = False And objmixed(4) = False And objmixed(5) = False And objmixed(6) = False And objmixed(7) = False And objmixed(8) = True And objmixed(9) = False And objmixed(10) = False And objmixed(11) = True And objmixed(12) = False And objmixed(13) = False And objmixed(14) = False And objmixed(15) = False And objmixed(16) = False And objmixed(17) = True And objmixed(18) = True And objmixed(19) = False And objmixed(20) = False Then
                    Console.WriteLine(", you then take it out of the oven and wait for it to cool.")
                End If
            Else
                Console.WriteLine("you only need to use the oven once")
            End If
        Else
            Console.WriteLine("Place something inside the beaker first.")
        End If
    End Sub

    Sub take()
        For i = 0 To objnum
            If UCase(objname(i)) = UCase(System.Text.RegularExpressions.Regex.Split(command, commandWords(0) + " ")(1)) Then

                If objlocation(i) = location And objget(i) = True Then
                    objlocation(i) = 0
                    Console.WriteLine("You picked up the " & objname(i) & ".")
                ElseIf objlocation(i) = location And objget(i) = False
                    Console.WriteLine(objgetmessage(i))
                ElseIf objlocation(i) = 0
                    Console.WriteLine("Item is already in inventory.")
                Else
                    Console.WriteLine("Item is not here.")
                End If
            End If
        Next
    End Sub

    Sub examine()
        For i = 0 To objnum
            If UCase(objname(i)) = UCase(System.Text.RegularExpressions.Regex.Split(command, commandWords(0) + " ")(1)) And (objlocation(i) = location Or objlocation(i) = 0) Then
                Console.WriteLine(objdesc(i))
            End If
        Next
        If UCase(commandWords(1)) = "CABINET" And objlocation(3) = location Then
            objget(3) = True
        End If
    End Sub

    Sub punch()
        For i = 0 To objnum
            If UCase(objname(i)) = UCase(System.Text.RegularExpressions.Regex.Split(command, commandWords(0) + " ")(1)) And (objlocation(i) = location Or objlocation(i) = 0) Then
                Console.WriteLine(objpunch(i))
            End If
        Next
        If UCase(commandWords(1)) = "PILLS" And (objlocation(8) = location Or objlocation(8) = 0) And pills = True Then
            objname(8) = "Ephedrine"
            objname2(8) = "Ephedrine"
            objdesc(8) = "'Ephedrine -for making meth'"
            objpunch(8) = "You punch the ephedrine and... nothing happens!"
            pills = False

        End If
        If UCase(commandWords(1)) = "PIZZA" And objlocation(16) = location Then
            objpunch(16) = "Further pizza abuse will not be tolerated."
            objdesc(16) = "Some kind of circular bread with tomato puree, cheese and circular meat slices... With a hole through it."
        End If
    End Sub

    Sub cut()
        If UCase(commandWords(1)) = "HOSE" And location = 3 And hosecut = False Then
            If objlocation(15) = 0 Then
                hosecut = True
                objget(14) = True
                Console.WriteLine("You cut the shower hose off (don't question it).")
            Else
                Console.WriteLine("You don't have anything to cut it with.")
            End If
        ElseIf UCase(commandWords(1)) = "SELF" Or UCase(commandWords(1)) = "WRISTS"
            If objlocation(15) = 0 Then
                Console.WriteLine("You cut your wrists with the kitchen knife. Good ridance.")
                playing = False
                Console.Title = "Game End"
                Console.ReadLine()
            Else
                Console.WriteLine("You decide to end it all. You claw at you wrists with your fingernails. Good ridance.")
                playing = False
                Console.Title = "Game End"
                Console.ReadLine()
            End If
        End If
    End Sub

    Function tryMoving(direction As String) As Boolean
        ' returns true if the direction parameter is a valid direction to exit this room in
        ' returns false otherwise
        If InStr(exits(location), UCase(Left(direction, 1))) > 0 Then
            Select Case UCase(Left(direction, 1))
                Case "N"
                    location = location - 3
                Case "S"
                    location = location + 3
                Case "E"
                    location = location + 1
                Case "W"
                    location = location - 1
                Case "U"
                    location = location - 5
                Case "D"
                    location = location + 5
            End Select
            Return True
        End If
        Return False
    End Function

    Sub lightoff()
        If openblinds = False Then
            Console.WriteLine("If you did that you wouldn't be able to see!")
        ElseIf lighton = False
            Console.WriteLine("The light is already off.")
        Else
            Console.WriteLine("You turn the light off.")
            lighton = False
            desc(5) = "You are in the bedroom. The window opposite to a large bed illuminates the room."
            displayRoom()
        End If
    End Sub

    Sub unlock()
        If (UCase(commandWords(1)) = "LOCK" Or UCase(commandWords(1)) = "CUPBOARD") And location = 7 Then

            If objlocation(3) = 0 Then

                If locked = True Then
                    Console.WriteLine("You unlock the lock.")
                    locked = False
                    objget(13) = True
                Else
                    Console.WriteLine("The lock is already unlocked.")
                End If
            Else
                Console.WriteLine("You don't have a key.")
            End If
        End If
    End Sub



    Sub initialise()
        location = 7
        ' ' ' ' ' ' ' '
        ' ROOM NAMES  '
        ' ' ' ' ' ' ' '
        room(0) = "Inventory" ' also doubles as the inventory
        room(1) = "Shed"
        room(2) = ""
        room(3) = "Bathroom"
        room(4) = "Garden"
        room(5) = "Bedroom"
        room(6) = "Landing"
        room(7) = "Kitchen"
        room(8) = "Living Room"
        room(9) = ""
        room(10) = "Garage"
        room(11) = "Hallway"
        room(12) = ""
        ' ' ' ' ' ' ' ' ' ' '
        ' ROOM DESCRIPTIONS '
        ' ' ' ' ' ' ' ' ' ' '
        desc(0) = "Inventory"
        desc(1) = "You are in the shed containing broken gardening equipment and rusting bikes. The door is to your south."
        desc(2) = ""
        desc(3) = "You are in the bathroom. The dim light from the landing only shows the empty light bulb socket"
        desc(4) = "You stand in the middle of the overgrown garden. A rotting shed lies to your north. To your south is the back door to the house."
        desc(5) = "You are in the bedroom. The light bulb above illuminates the room. Blinds cover the window opposite to a large bed."
        desc(6) = "You are on the landing. It is dimly lit, you can make out doors to your north and west as well as stairs going down."
        desc(7) = "You are in the kitchen. You notice a large lock on the cupboard under the sink. There is an oven here. There is a beaker here. The garden is to your north, the living room is to your east."
        desc(8) = "You are in the living room. In the middle of the room there is a leather sofa which faces a flatscreen T.V. You see a pizza box. To your south is the hallway. To your west is the door to the kitchen."
        desc(9) = ""
        desc(10) = "You are in the garage. A red coupe is infront of you with its bonnet lifted up."
        desc(11) = "You are in the tight hallway. To your south is the battered wooden front door. To the west is the door to the garage. North is the way to the living room. The foot of the stair case and a large cabinet are right next to you."
        desc(12) = ""

        ' EXITS '
        ' ' ' ' '
        exits(0) = ""
        exits(1) = "S"
        exits(2) = ""
        exits(3) = "S"
        exits(4) = "NS"
        exits(5) = "E"
        exits(6) = "NWD"
        exits(7) = "NE"
        exits(8) = "SW"
        exits(9) = ""
        exits(10) = "E"
        exits(11) = "NWU"
        exits(12) = ""

        objname(0) = "Blinds"
        objname(1) = "Lightbulb"
        objname(2) = "Cabinet"
        objname(3) = "Key"
        objname(4) = "Car"
        objname(5) = "Cannister"
        objname(6) = "Salt"
        objname(7) = "Cupboard"
        objname(8) = "Pills"
        objname(9) = "Drain Cleaner"
        objname(10) = "Paint Thinner"
        objname(11) = "Phosphurus"
        objname(12) = "Cupboard"
        objname(13) = "Lock"
        objname(14) = "Hose"
        objname(15) = "Knife"
        objname(16) = "Pizza"
        objname(17) = "Acid"
        objname(18) = "Iodine"
        objname(19) = "Recipe"
        objname(20) = "Oven"
        objname(21) = "Beaker"

        objname2(0) = "Blinds"
        objname2(1) = "a Lightbulb"
        objname2(2) = "a Cabinet"
        objname2(3) = "a Key"
        objname2(4) = "a Car"
        objname2(5) = "a Cannister"
        objname2(6) = "Salt"
        objname2(7) = "a Cupboard"
        objname2(8) = "Pills"
        objname2(9) = "Drain Cleaner"
        objname2(10) = "Paint Thinner"
        objname2(11) = "Phosphurus"
        objname2(12) = "a Cupboard"
        objname2(13) = "a Lock"
        objname2(14) = "a Hose"
        objname2(15) = "a Knife"
        objname2(16) = "a Pizza"
        objname2(17) = "Acid"
        objname2(18) = "Iodine"
        objname2(19) = "a Recipe"
        objname2(20) = "a Oven"
        objname2(21) = "a Beaker"

        objlocation(0) = 5
        objlocation(1) = 5
        objlocation(2) = 11
        objlocation(3) = 11
        objlocation(4) = 10
        objlocation(5) = 10
        objlocation(6) = 10
        objlocation(7) = 3
        objlocation(8) = 3
        objlocation(9) = 4
        objlocation(10) = 1
        objlocation(11) = 8
        objlocation(12) = 7
        objlocation(13) = 7
        objlocation(14) = 3
        objlocation(15) = 7
        objlocation(16) = 8
        objlocation(17) = 7
        objlocation(18) = 1
        objlocation(19) = 7
        objlocation(20) = 7
        objlocation(21) = 7

        objdesc(0) = ""
        objdesc(1) = ""
        objdesc(2) = "The Cabinet is scratched and worn, you think it's made of oak. You examine the side and notice a key behind it."
        objdesc(3) = ""
        objdesc(4) = "You look under Bonnet, you see the Engine and Battery."
        objdesc(5) = "The Cannister is empty, it faintly smells of petrol."
        objdesc(6) = "'De-Icing Salt'"
        objdesc(7) = "Maybe instead of looking at the cupboard you could open it."
        objdesc(8) = "'Cold Pills -pills for colds'"
        objdesc(9) = "On the back it reads 'Sodium Hydroxide'"
        objdesc(10) = "On the back it reads 'Xylene'"
        objdesc(11) = "It reads 'Phosphurus'"
        objdesc(12) = "Very cupboard like!"
        objdesc(13) = "It's a lock, you need a key, I'm not sure what information you thoguht you would get from this."
        objdesc(14) = "A hose of the shower variety."
        objdesc(15) = "Pointy and stabby."
        objdesc(16) = "Some kind of circular bread with tomato puree on top of which lies a thin layer of cheese and circular meat slices."
        objdesc(17) = "The label reads 'Hydrochloric Acid'."
        objdesc(18) = "It certainly looks like Iodine."
        objdesc(19) = "'First, punch the cold pills into ephedrine powder. Then mix the ephedrine with phosphurus, iodine and hydrochloric acid. Place the mixture in the oven for an hour and then leave to cool. Next add sodium hydroxide and xylene. Find a cannister and mix salt and battery acid inside it; this will create a gas. Use a hose to funnel the gas into from the cannister into your mixture.'"
        objdesc(20) = "The hot thingy."
        objdesc(21) = "Nothing to do with beaks."

        objget(0) = False
        objget(1) = False
        objget(2) = False
        objget(3) = False
        objget(4) = False
        objget(5) = True
        objget(6) = True
        objget(7) = False
        objget(8) = False
        objget(9) = True
        objget(10) = True
        objget(11) = False
        objget(12) = False
        objget(13) = False
        objget(14) = False
        objget(15) = True
        objget(16) = True
        objget(17) = False
        objget(18) = True
        objget(19) = True
        objget(20) = False
        objget(21) = False

        objgetmessage(0) = ""
        objgetmessage(1) = "The lightbulb is too hot to touch."
        objgetmessage(2) = "Do you want to break your back?"
        objgetmessage(3) = ""
        objgetmessage(4) = ""
        objgetmessage(5) = ""
        objgetmessage(6) = ""
        objgetmessage(7) = "What are you planning to do with that?"
        objgetmessage(8) = ""
        objgetmessage(9) = ""
        objgetmessage(10) = ""
        objgetmessage(11) = ""
        objgetmessage(12) = "You really just tried to pick up a a sink cupboard?"
        objgetmessage(13) = "That's not how locks work..."
        objgetmessage(14) = "You try to pull it off but you fail, maybe you could try cutting it."
        objgetmessage(15) = ""
        objgetmessage(16) = ""
        objgetmessage(17) = ""
        objgetmessage(18) = ""
        objgetmessage(19) = ""
        objgetmessage(20) = "It's not portable."
        objgetmessage(21) = "Might as well just leave it there."

        objpunch(0) = "You swing an uppercut at the blinds, you really showed them who's boss."
        objpunch(1) = "You throw a dainty jab at the lightbulb. GOOD JOB!"
        objpunch(2) = "You sum up all your strength to throw a right hook at the cabinet as revenge for all the pain and suffering its kind has caused you."
        objpunch(3) = "You punch the key... weirdo."
        objpunch(4) = "I refuse to let you do that."
        objpunch(5) = "You swing a punch at the cannister sending it flying across the floor."
        objpunch(6) = "You use the salt as a punching bag, it's not like you need to make meth or anything."
        objpunch(7) = "Stop punching things and make meth."
        objpunch(8) = "You send a flurry of punches into the pills crushing them into ephedrine."
        objpunch(9) = "You need that."
        objpunch(10) = "You need that."
        objpunch(11) = "You need that."
        objpunch(12) = "You have a job to do..."
        objpunch(13) = "You're not Chuck Norris."
        objpunch(14) = "No."
        objpunch(15) = "I feel like you may have something wrong with your head."
        objpunch(16) = "You punch a hole through the pizza. What that achieved i do not know."
        objpunch(17) = "Your blow breaks the bottle and hydrochloric acid sprays out, melting you."
        objpunch(18) = "You need that."
        objpunch(19) = "Damaging that couldn't possibly be a problem."
        objpunch(20) = "You throw a Haymaker at the oven. You successfully bruised you knuckles."
        objpunch(21) = "Don't punch objects made of glass."

    End Sub




End Module
