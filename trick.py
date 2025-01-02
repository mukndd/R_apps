from time import sleep

original_card_list = ["clubs_king","hearts_king","spades_king","diamonds_king","clubs_queen","hearts_queen","spades_queen","diamonds_queen"]
colors = {"spades":"black","clubs":"black","hearts":"red","diamonds":"red"}
original_card_list1 = [1,2,3,4,5,6,7,8]
left_cards = []
right_cards = []
print()
print(original_card_list)
print()
print("Choose a card from the above list and remember it")
print()
chosen = input("Did you choose a card?\n")
print()
print("okay , lets start then")
print()
for i in original_card_list:
    if original_card_list.index(i)%2==0:
        right_cards.append(i)
    else:
        left_cards.append(i)
print("Cards to show - ",left_cards)
print()
question1 = input("Do you see your chosen card in this? y/n \n")
if question1=='y':
    original_card_list = right_cards+left_cards
else:
    original_card_list = left_cards+right_cards

left_cards = []
right_cards = []
for i in original_card_list:
    if original_card_list.index(i)%2==0:
        left_cards.append(i)
    else:
        right_cards.append(i)
left_cards.reverse()
right_cards.reverse()
print("Cards to show - ",left_cards)
print()
question2 = input("Do you see your chosen card in this? y/n \n")
if question2=='y':
    original_card_list = left_cards+right_cards
else:
    original_card_list = right_cards+left_cards

left_cards = []
right_cards = []
for i in original_card_list:
    if original_card_list.index(i)%2==0:
        right_cards.append(i)
    else:
        left_cards.append(i)
print("Cards to show - ",left_cards)
print()
question2 = input("Do you see your chosen card in this? y/n \n")
if question2=='y':
    original_card_list = left_cards+right_cards
else:
    original_card_list = right_cards+left_cards

list1 =[]
list2=[]
list3=[]
b = ''
a = ''
for i in original_card_list:
    if original_card_list.index(i)%2==0:
        list1.append(i)
    else:
        list2.append(i)
for i in list1:
    if list1.index(i)%2==0:
        list3.append(i)
d = list3[1]
e = d.split("_")
ee = e[0]
a = list3[0]
sleep(2)
aa = list2[0].split("_")
print("The gender of your card is -",aa[1])
sleep(2)
zz = list1[0]
bb = zz.split("_")
cc = bb[0]
dd = colors[f"{cc}"]
print("the color of your card is -",dd)
sleep(2)
print("the suit of your card is -",ee)
sleep(2)
print("Hence the card you chose was -",a)
