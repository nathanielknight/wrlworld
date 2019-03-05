/// This module contains a function for generating Perlin noise-based grids.


function assert(cond: boolean, msg: string): void {
    if (!cond) {
        throw new Error(msg);
    }
}

// Linear interploation between a0 and a1.
function lerp(a0: number, a1: number, w: number): number {
    return (1.0 - w) * a0 + w * a1;
}

type Point = [number, number];

function randomUnitPoint(): Point {
    let x = 2 * (Math.random() - 0.5);
    let y = 2 * (Math.random() - 0.5);
    let mag = Math.sqrt(x * x + y * y);
    return [x / mag, y / mag];
}

class GradGrid {
    /// 2-D random gradient grid.
    xpts: Float32Array;
    ypts: Float32Array;
    stride: number;

    constructor(xsize: number, ysize: number) {
        // Calculate grid params
        this.stride = ysize + 1;
        let arraySize = this.stride * (xsize + 1);
        // Allocate x and y memory
        this.xpts = new Float32Array(arraySize);
        this.ypts = new Float32Array(arraySize);
        // Initialize array
        for (var i = 0; i < arraySize; i++) {
            let [x, y] = randomUnitPoint();
            this.xpts[i] = x;
            this.ypts[i] = y;
        }
    }

    public get(ix: number, iy: number): Point {
        let idx = ix + this.stride * iy;
        let x = this.xpts[idx];
        let y = this.ypts[idx];
        return [x, y];
    }
}

export class Perlin {
    private xsize: number;
    private ysize: number;
    private gradient: GradGrid;

    private static scale: number = 2.2;

    public constructor(xsize: number, ysize: number) {
        this.xsize = xsize;
        this.ysize = ysize;
        this.gradient = new GradGrid(xsize, ysize);
    }

    // Take the dot-product of a point and gradient vector.
    private dotGridGradient(ix: number, iy: number, x: number, y: number): number {
        let dx = x - ix;
        let dy = y - iy;
        let [gx, gy] = this.gradient.get(ix, iy);
        return (dx * gx + dy * gy);
    }

    private pointOf(x: number, y: number): Point {
        return [Math.floor(x), Math.floor(y)];
    }

    private checkX(x: number): number {
        assert(0 <= x && x < this.xsize, "Perlin: x range check");
        return x;
    }

    private checkY(y: number): number {
        assert(0 <= y && y < this.ysize, "Perlin: y range check");
        return y;
    }

    public get(_x: number, _y: number): number {
        // Check bounds
        let x = this.checkX(_x);
        let y = this.checkY(_y);


        // Grid points of input point.
        let [ix0, iy0] = this.pointOf(x, y);
        let ix1 = ix0 + 1;
        let iy1 = iy0 + 1;

        // Interpolation weights.
        let sx = x - ix0;
        let sy = y - iy0;

        // Interpolate between grid point gradients.
        let n0 = this.dotGridGradient(ix0, iy0, x, y);
        let n1 = this.dotGridGradient(ix1, iy0, x, y);
        let g0 = lerp(n0, n1, sx);

        let n2 = this.dotGridGradient(ix0, iy1, x, y);
        let n3 = this.dotGridGradient(ix1, iy1, x, y);
        let g1 = lerp(n2, n3, sx);

        return lerp(g0, g1, sy) * Perlin.scale;
    }
}