/* Licensed under the MIT/X11 license.
 * Copyright (c) 2006-2008 the OpenTK Team.
 * This notice may not be removed from any source distribution.
 * See license.txt for licensing detailed licensing details.
 *
 * Contributions by Andy Gill, James Talton and Georg Wächter.
 */

using System;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Contains common mathematical constants.
    /// Copied from - https://github.com/opentk/opentk/blob/3.x/src/OpenTK/Math/MathHelper.cs
    /// </summary>
    public static class MathF
    {
        /// <summary>
        /// Defines the value of Pi as a <see cref="System.Single"/>.
        /// </summary>
        public const float PI = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930382f;

        /// <summary>
        /// Defines the value of Pi divided by two as a <see cref="System.Single"/>.
        /// </summary>
        public const float PI_OVER_2 = PI / 2;

        /// <summary>
        /// Defines the value of Pi divided by three as a <see cref="System.Single"/>.
        /// </summary>
        public const float PI_OVER_3 = PI / 3;

        /// <summary>
        /// Definesthe value of  Pi divided by four as a <see cref="System.Single"/>.
        /// </summary>
        public const float PI_OVER_4 = PI / 4;

        /// <summary>
        /// Defines the value of Pi divided by six as a <see cref="System.Single"/>.
        /// </summary>
        public const float PI_OVER_6 = PI / 6;

        /// <summary>
        /// Defines the value of Pi multiplied by two as a <see cref="System.Single"/>.
        /// </summary>
        public const float TWO_PI = 2 * PI;

        /// <summary>
        /// Defines the value of Pi multiplied by 3 and divided by two as a <see cref="System.Single"/>.
        /// </summary>
        public const float THREE_OVER_PI = 3 * PI / 2;

        /// <summary>
        /// Defines the value of E as a <see cref="System.Single"/>.
        /// </summary>
        public const float E = 2.71828182845904523536f;

        /// <summary>
        /// Defines the base-10 logarithm of E.
        /// </summary>
        public const float LOG_10_E = 0.434294482f;

        /// <summary>
        /// Defines the base-2 logarithm of E.
        /// </summary>
        public const float LOG_2_E = 1.442695041f;
    }
}