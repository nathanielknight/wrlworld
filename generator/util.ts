function clamp(a: number, b: number, x: number): number {
    if (x <= a) {
        return a;
    }
    if (b <= x) {
        return b;
    }
    return x;
}

function assert(p: boolean, m?: string): void {
    if (!p) {
        throw new Error(m);
    }
}

function test(name: string, p: () => void): void {
    try {
        p();
    } catch (e) {
        console.error(`${name}: ${e}`);
        return;
    }
    console.log(`${name}: passed`);
}


