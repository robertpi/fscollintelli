#light
namespace Strangelights.HierarchicalClustering
open System
open System.Globalization
open System.Windows
open System.Windows.Media


type Dendrogram(t) =
    inherit FrameworkElement()
    let rec getHeight t =
        match t with
        | Node { Left = l; Right = r } -> 
            (getHeight l.NodeDetails) + (getHeight r.NodeDetails)
        | Leaf x -> 1.
    let getDepth t =
        let rec getDepthInner t d =
            match t with
            | Node { Left = l; Right = r } ->
                max
                    (getDepthInner l.NodeDetails d + 1.0)
                    (getDepthInner r.NodeDetails d + 1.0)
            | Leaf x -> d
        getDepthInner t 0.0
    let raise2ToPower (x : float) =
        Math.Pow(2.0, Convert.ToDouble(x))
    override x.OnRender(dc: DrawingContext) =
        let height = getHeight t * 20.
        let width = 1200.
        let depth = getDepth t
        
        let scaling = width - 150. / depth 
        
        let rec drawNode t x y =
            let h1 = (getHeight t.Left.NodeDetails) * 20.
            let h2 = (getHeight t.Right.NodeDetails) * 20.
            let top = y - (h1 + h2) / 2.
            let bottom = y + (h1 + h2) / 2.
            
            let ll = t.Distance * scaling
            ()
        // constants that relate to the size and position
        // of the tree
        let center = base.Width / 2.0
        let maxWidth = 32.0 * raise2ToPower (getDepth t)
        // function for drawing a leaf node
        let drawLeaf (x : float) (y : float) v =
            let text = new FormattedText(v, CultureInfo.GetCultureInfo("en-us"),
                                         FlowDirection.LeftToRight,
                                         new Typeface("Verdana"),
                                         8., Brushes.Black)
            dc.DrawText(text, new Point(x, y))
        let pen = new Pen(Brushes.Black, 1.0)
        // draw a connector between the nodes when necessary
        let connectNodes x y p =
            match p with
            | Some(px, py) -> dc.DrawLine(pen, new Point(px, py), new Point(x, y))
            | None -> ()
        // the main function to walk the tree structure drawing the
        // nodes as we go
        let rec drawTreeInner t d w p =
            let x = center - (maxWidth * w)
            let y = d * 32.0
            connectNodes x y p
            match t with
            | Node { Left = l; Right = r } ->
                dc.DrawEllipse(Brushes.Black, pen, new Point(x - 3.0, y - 3.0), 7.0, 7.0)
                let d = (d + 1.0)
                drawTreeInner l.NodeDetails d (w + (1.0 / d)) (Some(x, y))
                drawTreeInner r.NodeDetails d (w - (1.0 / d)) (Some(x, y))
            | Leaf v -> drawLeaf x y v
        drawTreeInner t 0.0 0.0 None


