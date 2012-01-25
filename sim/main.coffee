
class Color
    constructor: ()->
        ch=()->
            Math.floor(Math.random()*255)
        @r=ch()
        @g=ch()
        @b=ch()
    
    with_alpha: (alpha)->
        "rgba(#{@r},#{@g},#{@b},#{alpha})"

new_id=0
draw_chart={}

class Chart
    constructor: ()->
        # network
        @id=new_id++
        @links=[]
        @inbox=[]
        
        # world
        @power=((0 for j in [-5..5]) for i in [-5..5])
        
        # debug: visualization
        @color=new Color()
        
        # embed in time
        console.log 'node',@id,'is created'
        @step()
    
    step: ()->
        for msg in @inbox
            switch msg.type
                when 'space'
                    if msg.from.id!=@id
                        ix=Math.round msg.px
                        iy=Math.round msg.py
                        if -5<=ix and ix<=5 and -5<=iy and iy<=5
                            if msg.power>@power[ix+5][iy+5]
                                @power[ix+5][iy+5]*=0.5
                                @update_connection msg.from, msg.ox, msg.oy

    #                    else
                        
                        @broadcast msg
        
        @inbox=[]
        @arbitrate()
        
        # spawn
        if document.next_spawn and Math.random()<0.5
            document.next_spawn=false
            @spawn_random()
        
        dt=50+Math.floor(Math.random()*10)
        setTimeout (()=>@step()), dt
    
    optimize_connection: ()->
        n=3
        if @links.length<n
            return
        else
            ls=@links.sort (a,b)->(Math.abs(a.dx)+Math.abs(a.dy))-(Math.abs(b.dx)+Math.abs(b.dy))
            for node in ls[n...ls.length]
                @disconnect node.chart
    
    update_connection: (ch,dx,dy)->
        for node in @links
            if node.chart.id==ch.id
                node.dx=dx
                node.dy=dy
                return

        console.log 'connecting',@id,'and',ch.id
        @links.push
            dx: dx
            dy: dy
            chart: ch
        
        ch.links.push
            dx: -dx
            dy: -dy
            chart: this
    
    disconnect: (ch)->
        console.log 'disconnecting',@id,'and',ch.id
        @links=(node for node in @links when node.chart.id!=ch.id)
        ch.links=(node for node in ch.links when node.chart.id!=@id)
        
    arbitrate: ()->
        @optimize_connection()
        @power[5][5]=1
        
        # diffuse power
        for i in [-4..4]
            for j in [-4..4]
                di=Math.floor(Math.random()*3)-1
                dj=Math.floor(Math.random()*3)-1
                v0=@power[i+5][j+5]
                v1=@power[i+di+5][j+dj+5]
                @power[i+5][j+5]=v0*0.95+v1*0.05
                @power[i+di+5][j+dj+5]=v0*0.05+v1*0.95
        
        # amplify power
        enhance=(x)->
            1-(1-x)*(1-x)
        
        for i in [-5..5]
            for j in [-5..5]
                @power[i+5][j+5]*=0.99 #=enhance @power[i+5][j+5]
        
        # advertise
        for i in [-5..5]
            for j in [-5..5]
                @broadcast
                    type: 'space'
                    prev: this
                    from: this
                    ttl: 3
                    ox: 0
                    oy: 0
                    px: i
                    py: j
                    power: @power[i+5][j+5]
    
    broadcast: (msg)->
        for node in @links
            switch msg.type
                when 'space'
                    if node.chart.id!=msg.prev.id and msg.ttl>0
                        node.chart.inbox.push
                            type: 'space'
                            prev: this
                            ttl: msg.ttl-1
                            from: msg.from
                            ox: msg.ox-node.dx
                            oy: msg.oy-node.dy
                            px: msg.px-node.dx
                            py: msg.py-node.dy
                            power: msg.power
    
    spawn_random: ()->
     #   @power=((0 for j in [-5..5]) for i in [-5..5])
        
        # select an origin in current chart
        px=(Math.random()-0.5)*11
        py=(Math.random()-0.5)*11
        
        @update_connection new Chart(), px, py
    
    # impure method to visualize atlas structure itself
    draw: (ctx,from=null)->
        if from==null
            draw_chart={}
        else if draw_chart[@id]!=undefined
            return
    
        draw_chart[@id]=true
        
        for i in [-5..5]
            for j in [-5..5]
                ctx.fillStyle=@color.with_alpha @power[i+5][j+5]*2
                ctx.beginPath()
                ctx.arc i, j, 0.2, 0, 2*Math.PI
                ctx.fill()
        
        for node in @links
            if node.chart.id!=from?.id
                ctx.beginPath()
                ctx.moveTo 0, 0
                ctx.lineTo node.dx, node.dy
                ctx.lineWidth=0.1
                ctx.strokeStyle='rgba(0,0,0,0.5)'
                ctx.stroke()
                
                ctx.save()
                ctx.translate node.dx, node.dy
                node.chart.draw ctx, this
                ctx.restore()
    

$ ->
    document.next_spawn=false
    document.spawn_somewhere=()->
        document.next_spawn=true
    
    # setup view
    cv=$('#main')[0]
    ctx=cv.getContext '2d'

    
    # peek into atlas from current chart
    curr=new Chart()

    draw=()->
        # visualize atlas
        ctx.save()
        ctx.fillStyle='white'
        ctx.fillRect 0, 0, cv.width, cv.height
        ctx.translate cv.width*0.5, cv.height*0.5
        ctx.scale 10, 10
        curr.draw ctx
        ctx.restore()
        
        # visualize link
        
        setTimeout draw, 50
    
    draw()

