declare function getNoise(x: number, y: number): number;
declare function seedNoise(seed: number): void;

class NoiseMap2D extends ArrayMap2D {
    private seed: number;
    readonly scale: number;

    constructor(xsize: number, ysize: number, scale: number) {
        super(xsize, ysize);
        this.seed = Math.random();
        this.scale = scale;
        seedNoise(this.seed);
        this.genMap();
    }

    private genMap() {
        for (let idx = 0; idx < this.size; idx++) {
            let [i, j] = this.coordsOf(idx);
            this.map[idx] = getNoise(i * this.scale, j * this.scale);
        }
    }

    public fcombine(fn: (a: number, b: number) => number, other: ArrayMap2D): ArrayMap2D {
        assert(this.xsize === other.xsize, "X-sizes of ArrayMap2D must match");
        assert(this.ysize === other.ysize, "Y-sizes of ArrayMap2D must match");

        let result = new ArrayMap2D(this.xsize, this.ysize);

        for (let idx = 0; idx < this.size; idx++) {
            result.map[idx] = fn(this.map[idx], other.map[idx]);
        }
        return result;
    }
}


function testNoiseMap2D() {
    let cnv: HTMLCanvasElement = <HTMLCanvasElement>document.getElementById("testcanvas");
    let ctx = cnv.getContext("2d");
    if (ctx == null) {
        throw new Error("Failed to get test context");
    }
    let imdata = ctx.getImageData(0, 0, 256, 256);
    let nm = new NoiseMap2D(256, 256, 0.015);
    for (let idx = 0; idx < nm.size; idx++) {
        let dataidx = idx * 4;
        let val = nm.getIdx(idx);
        let alpha = 255 * (val + 1) / 2;
        imdata.data[dataidx] = 0;
        imdata.data[dataidx + 1] = 0;
        imdata.data[dataidx + 2] = 0;
        imdata.data[dataidx + 3] = alpha;
    }

    ctx.putImageData(imdata, 0, 0);

}
