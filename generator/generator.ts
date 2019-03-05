const XSIZE: number = 256;
const YSIZE: number = 256;

class Generator {

    private elevation: ArrayMap2D;
    private size: number;
    constructor() {
        this.elevation = new NoiseMap2D(XSIZE, YSIZE, 0.014);
        this.size = this.elevation.size;
    }

    public drawToContext(ctx: CanvasRenderingContext2D): void {
        let imdata = ctx.getImageData(0, 0, XSIZE, YSIZE);
        for (let idx = 0; idx < this.size; idx++) {
            let didx = 4 * idx;
            let rgba = this.rgba(idx);
            for (let i = 0; i < 4; i++) {
                imdata.data[didx + i] = rgba[i];
            }
        }
        ctx.putImageData(imdata, 0, 0);
    }

    private rgba(idx: number): [number, number, number, number] {
        let v = this.elevation.getIdx(idx);
        let b = v < 0 ? 180 : 0;
        let g = v >= 0 ? 180 : 0;
        return [0, g, b, 255];
    }

}


function testGenerator() {
    let g = new Generator();
    let e = <HTMLCanvasElement>document.getElementById("testcanvas");
    let ctx = e.getContext("2d");
    if (ctx == null) {
        throw new Error("Couldn't get test canvas");
    }
    g.drawToContext(ctx);
}