import { deepEqual } from 'fast-equals';

export { deepEqual };

if (typeof window !== 'undefined') {
    window.deepEqual = deepEqual;
}
