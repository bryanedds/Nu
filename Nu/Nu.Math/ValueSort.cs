using System;

namespace Nu
{
    /// <summary>
    /// Compares two values by reference.
    /// </summary>
    public delegate int ValueComparison<T>(ref T left, ref T right) where T : struct;

    /// <summary>
    /// Enables faster sorting of non-trivial values types.
    /// </summary>
    public static class ValueSort
    {
        private static void Swap<T>(T[] a, int i, int j) where T : struct
        {
            if (i != j)
            {
                T val = a[i];
                a[i] = a[j];
                a[j] = val;
            }
        }

        private static void SwapIfGreater<T>(T[] keys, ValueComparison<T> comparer, int a, int b) where T : struct
        {
            if (a != b && comparer(ref keys[a], ref keys[b]) > 0)
            {
                T val = keys[a];
                keys[a] = keys[b];
                keys[b] = val;
            }
        }

        private static int FloorLog2(int n)
        {
            int num = 0;
            while (n >= 1)
            {
                num++;
                n /= 2;
            }
            return num;
        }

        private static void DownHeap<T>(T[] keys, int i, int n, int lo, ValueComparison<T> comparer) where T : struct
        {
            T val = keys[lo + i - 1]; // copy T
            while (i <= n / 2)
            {
                int num = 2 * i;
                if (num < n && comparer(ref keys[lo + num - 1], ref keys[lo + num]) < 0) num++;
                if (comparer(ref val, ref keys[lo + num - 1]) >= 0) break;
                keys[lo + i - 1] = keys[lo + num - 1];
                i = num;
            }
            keys[lo + i - 1] = val;
        }

        private static void Heapsort<T>(T[] keys, int lo, int hi, ValueComparison<T> comparer) where T : struct
        {
            int num = hi - lo + 1;
            for (int num2 = num / 2; num2 >= 1; num2--)
            {
                DownHeap(keys, num2, num, lo, comparer);
            }
            for (int num3 = num; num3 > 1; num3--)
            {
                Swap(keys, lo, lo + num3 - 1);
                DownHeap(keys, 1, num3 - 1, lo, comparer);
            }
        }

        private static void InsertionSort<T>(T[] keys, int lo, int hi, ValueComparison<T> comparer) where T : struct
        {
            for (int i = lo; i < hi; i++)
            {
                int num = i;
                T val = keys[i + 1]; // copy T
                while (num >= lo && comparer(ref val, ref keys[num]) < 0)
                {
                    keys[num + 1] = keys[num];
                    num--;
                }
                keys[num + 1] = val;
            }
        }

        private static int PickPivotAndPartition<T>(T[] keys, int lo, int hi, ValueComparison<T> comparer) where T : struct
        {
            int num = lo + (hi - lo) / 2;
            SwapIfGreater(keys, comparer, lo, num);
            SwapIfGreater(keys, comparer, lo, hi);
            SwapIfGreater(keys, comparer, num, hi);
            T val = keys[num]; // copy T
            Swap(keys, num, hi - 1);
            int num2 = lo;
            int num3 = hi - 1;
            while (num2 < num3)
            {
                while (comparer(ref keys[++num2], ref val) < 0) ;
                while (comparer(ref val, ref keys[--num3]) < 0) ;
                if (num2 >= num3) break;
                Swap(keys, num2, num3);
            }
            Swap(keys, num2, hi - 1);
            return num2;
        }

        private static void IntroSort<T>(T[] keys, int lo, int hi, int depthLimit, ValueComparison<T> comparer) where T : struct
        {
            while (hi > lo)
            {
                int num = hi - lo + 1;
                if (num <= 16)
                {
                    switch (num)
                    {
                        case 1:
                            break;
                        case 2:
                            SwapIfGreater(keys, comparer, lo, hi);
                            break;
                        case 3:
                            SwapIfGreater(keys, comparer, lo, hi - 1);
                            SwapIfGreater(keys, comparer, lo, hi);
                            SwapIfGreater(keys, comparer, hi - 1, hi);
                            break;
                        default:
                            InsertionSort(keys, lo, hi, comparer);
                            break;
                    }
                    break;
                }
                if (depthLimit == 0)
                {
                    Heapsort(keys, lo, hi, comparer);
                    break;
                }
                depthLimit--;
                int num2 = PickPivotAndPartition(keys, lo, hi, comparer);
                IntroSort(keys, num2 + 1, hi, depthLimit, comparer);
                hi = num2 - 1;
            }
        }

        public static void IntroSort<T>(T[] keys, int left, int length, ValueComparison<T> comparer) where T : struct
        {
            if (length >= 2)
            {
                IntroSort(keys, left, length + left - 1, 2 * FloorLog2(keys.Length), comparer);
            }
        }

        public static void IntroSort<T>(T[] keys, ValueComparison<T> comparer) where T : struct
        {
            IntroSort(keys, 0, keys.Length - 1, 2 * FloorLog2(keys.Length), comparer);
        }
    }
}
