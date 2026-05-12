#互動版
!pip install ipywidgets

import random
import ipywidgets as widgets
from IPython.display import display, HTML, clear_output

#圖片
car_img = "https://images3.kingautos.net/2015/07/03/reeJNKMDuhDv2j7.jpg"
goat_img = "https://pic.nximg.cn/file/20231017/33760392_094552094120_2.jpg"


# =========================
# 設定全域變數
# =========================
doors = []
p1 = None
h1 = None
remain = None
game_stage = "choose"

door_box = widgets.Output()
message_box = widgets.Output()

choose_box = widgets.HBox()
decision_box = widgets.HBox()
restart_box = widgets.HBox()


# =========================
# 開始遊戲
# =========================
def start_game():
    global doors, p1, h1, remain, game_stage
    
    doors = ["GIFT", "GOAT", "GOAT"]
    random.shuffle(doors)

    p1 = None
    h1 = None
    remain = None
    game_stage = "choose"

    choose_box.layout.display = "flex"
    decision_box.layout.display = "none"

    with message_box:
        clear_output()
        print("Welcome to Monty Hall game <33")
        print("請先選擇一扇門")

    show_doors()


# =========================
# 顯示三扇門（翻開動畫）
# =========================
def show_doors(revealed_doors=None, animate=False):
    if revealed_doors is None:
        revealed_doors = []

    html = """
    <style>
    .door-container {
        display: flex;
        gap: 50px;
        margin-top: 40px;
        margin-bottom: 30px;
    }
    /*門的外觀*/
    .door-card {
        width: 130px;
        height: 180px;
        perspective: 1000px;
        text-align: center;
        font-family: "Microsoft JhengHei", "PingFang TC", Arial, sans-serif;
    }
    .door-inner {
        position: relative;
        width: 100%;
        height: 100%;
        transition: transform 1.0s;
        transform-style: preserve-3d;
    }
    .door-card.revealed .door-inner {
        transform: rotateY(180deg);
    }
    .door-front, .door-back {
        position: absolute;
        width: 100%;
        height: 100%;
        backface-visibility: hidden;
        border-radius: 12px;
        display: flex;
        justify-content: center;
        align-items: center;
        font-size: 22px;
        font-weight: bold;
        border: 3px solid #555;
    }
    .door-front {
        background: #0B1F3A;
        color: white;
    }
    .door-back {
        background: #F7F7F7;
        color: black;
        transform: rotateY(180deg);
    }
    
    /* 門背後的圖片 */
    .door-img {
        width: 100%;
        height: 100%;
        object-fit: cover;
    }
    /* 參賽者選到的門：粉色光暈*/
    .selected {
        box-shadow: 0 0 15px 5px pink;
    }
    /* 主持人選到的門：黃色光暈*/
    .opened {
        box-shadow: 0 0 15px 5px yellow;
    }
    </style>
    """

    html += '<div class="door-container">'

    for i in range(3):
        selected_class = "selected" if i == p1 else ""
        opened_class = "opened" if i == h1 else ""

        # animate=False：直接翻開
        # animate=True：先不要revealed 後面再JS加上去(動畫)
        if animate:
            revealed_class = ""
            animate_class = "to-reveal" if i in revealed_doors else ""
        else:
            revealed_class = "revealed" if i in revealed_doors else ""
            animate_class = ""

        #門後要顯示的東西
        if i in revealed_doors:
            if doors[i] == "GIFT":
                back_content = f'<img src="{car_img}" class="door-img">'
            else:
                back_content = f'<img src="{goat_img}" class="door-img">'
        else:
            back_content = "?"

        html += f"""
        <div id="door-{i}" class="door-card {revealed_class} {selected_class} {opened_class} {animate_class}">
            <div class="door-inner">
                <div class="door-front">Door {i + 1}</div>
                <div class="door-back">{back_content}</div>
            </div>
        </div>
        """

    html += "</div>"

    #JS延遲幫門加上revealedclass
    if animate and len(revealed_doors) > 0:
        html += """
        <script>
        setTimeout(function() {
            const doors = document.querySelectorAll('.to-reveal');
            doors.forEach((door, index) => {
                setTimeout(() => {
                    door.classList.add('revealed');
                }, index * 500);
            });
        }, 100);
        </script>
        """

    with door_box:
        clear_output()
        display(HTML(html))


# =========================
# 玩家第一次選門
# =========================
def choose_door(choice):
    global p1, h1, remain, game_stage

    if game_stage != "choose":
        return

    p1 = choice

    possible_open_doors = [
        i for i in range(3)
        if i != p1 and doors[i] == "GOAT"
    ]

    h1 = random.choice(possible_open_doors)

    remain = [
        i for i in range(3)
        if i != p1 and i != h1
    ][0]

    game_stage = "decide"

    choose_box.layout.display = "none"
    decision_box.layout.display = "flex"

    with message_box:
        clear_output()
        print(f"你選擇了第 {p1 + 1} 扇門")
        print(f"主持人打開第 {h1 + 1} 扇門...")
        print("選擇是否換門")

    # 主持人那扇門用翻開動畫
    show_doors(revealed_doors=[h1], animate=True)


# =========================
# 玩家決定換不換門
# =========================
def finish_game(switch):
    global game_stage

    if game_stage != "decide":
        return

    if switch:
        final_choice = remain
        decision = "換門"
    else:
        final_choice = p1
        decision = "不換門"

    game_stage = "end"
    decision_box.layout.display = "none"

    with message_box:
        clear_output()
        print(f"你選擇：{decision}")
        print(f"你最後選擇第 {final_choice + 1} 扇門")
        print("現在公布所有門的結果")

    # 三扇門全部翻開，做動畫
    show_doors(revealed_doors=[0, 1, 2], animate=True)

    with message_box:
        clear_output()
        print(f"你選擇：{decision}")
        print(f"你最後選擇第 {final_choice + 1} 扇門。")
        print("-" * 30)
        if doors[final_choice] == "GIFT":
            print("恭喜你得到禮物！")
        else:
            print("很可惜，你得到羊")


# =========================
# 建立按鈕
# =========================
door1_button = widgets.Button(description="選第 1 扇門")
door2_button = widgets.Button(description="選第 2 扇門")
door3_button = widgets.Button(description="選第 3 扇門")

door1_button.on_click(lambda b: choose_door(0))
door2_button.on_click(lambda b: choose_door(1))
door3_button.on_click(lambda b: choose_door(2))

choose_box.children = [door1_button, door2_button, door3_button]

keep_button = widgets.Button(description="不換門")
switch_button = widgets.Button(description="換門")

keep_button.on_click(lambda b: finish_game(False))
switch_button.on_click(lambda b: finish_game(True))

decision_box.children = [keep_button, switch_button]

restart_button = widgets.Button(description="重新開始")
restart_button.on_click(lambda b: start_game())

restart_box.children = [restart_button]


# =========================
# 顯示介面
# =========================
display(HTML("<h3>Step 1：請選擇一扇門</h3>"))
display(choose_box)

display(door_box)

display(HTML("<h3>Step 2：是否換門</h3>"))
display(decision_box)

display(message_box)

display(HTML("<h3>重新開始</h3>"))
display(restart_box)

# 開始遊戲
start_game()
