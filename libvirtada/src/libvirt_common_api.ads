pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;

package Libvirt_Common_Api is
   pragma Preelaborate;

   --  unsupported macro: VIR_DEPRECATED __attribute__((__deprecated__))
   --  unsupported macro: VIR_EXPORT_VAR extern
   LIBVIR_VERSION_NUMBER : constant := 4000000;  --  /usr/include/libvirt/libvirt-common.h:76
   --  arg-macro: function LIBVIR_CHECK_VERSION (major, minor, micro)
   --    return (major) * 1000000 + (minor) * 1000 + (micro) <= LIBVIR_VERSION_NUMBER;
   VIR_TYPED_PARAM_FIELD_LENGTH : constant := 80;  --  /usr/include/libvirt/libvirt-common.h:171
  -- * libvirt-common.h
  -- * Summary: common macros and enums for the libvirt and libvirt-admin library
  -- * Description: Provides common macros and enums needed by both libvirt and
  -- *              libvirt-admin libraries
  -- * Author: Erik Skultety <eskultet@redhat.com>
  -- *
  -- * Copyright (C) 2015 Red Hat, Inc.
  -- *
  -- * This library is free software; you can redistribute it and/or
  -- * modify it under the terms of the GNU Lesser General Public
  -- * License as published by the Free Software Foundation; either
  -- * version 2.1 of the License, or (at your option) any later version.
  -- *
  -- * This library is distributed in the hope that it will be useful,
  -- * but WITHOUT ANY WARRANTY; without even the implied warranty of
  -- * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  -- * Lesser General Public License for more details.
  -- *
  -- * You should have received a copy of the GNU Lesser General Public
  -- * License along with this library.  If not, see
  -- * <http://www.gnu.org/licenses/>.
  --  

  -- The feature is present in gcc-3.1 and newer.   
  -- General note - in the header files, any linear enumeration which
  -- * might be expanded in the future has an optional *_LAST value that
  -- * gives the size of the enum at the time of compilation, if the user
  -- * defines VIR_ENUM_SENTINELS.  Enumerations for bit values do not
  -- * have a *_LAST value, but additional bits may be defined.   

  -- library versioning  
  --*
  -- * LIBVIR_VERSION_NUMBER:
  -- *
  -- * Macro providing the version of the library as
  -- * version * 1,000,000 + minor * 1000 + micro
  --  

  --*
  -- * LIBVIR_CHECK_VERSION:
  -- * @major: major component of the version number
  -- * @minor: minor component of the version number
  -- * @micro: micro component of the version number
  -- *
  -- * Macro for developers to easily check what version of the library
  -- * their code is compiling against.
  -- * e.g.
  -- *   #if LIBVIR_CHECK_VERSION(1,1,3)
  -- *     // some code that only works in 1.1.3 and newer
  -- *   #endif
  --  

  -- * virFreeCallback:
  -- * @opaque: opaque user data provided at registration
  -- *
  -- * Type for a callback cleanup function to be paired with a callback.  This
  -- * function will be called as a final chance to clean up the @opaque
  -- * registered with the primary callback, at the time when the primary
  -- * callback is deregistered.
  -- *
  -- * It is forbidden to call any other libvirt APIs from an
  -- * implementation of this callback, since it can be invoked
  -- * from a context which is not re-entrant safe. Failure to
  -- * abide by this requirement may lead to application deadlocks
  -- * or crashes.
  --  

   type virFreeCallback is access procedure (opaque : System.Address);
   pragma Convention (C, virFreeCallback);  -- /usr/include/libvirt/libvirt-common.h:109

  -- Misc I/O error  
  -- End-of-file from server  
  -- Keepalive timer triggered  
  -- Client requested it  
   type virConnectCloseReason is 
     (VIR_CONNECT_CLOSE_REASON_ERROR,
      VIR_CONNECT_CLOSE_REASON_EOF,
      VIR_CONNECT_CLOSE_REASON_KEEPALIVE,
      VIR_CONNECT_CLOSE_REASON_CLIENT);
   pragma Convention (C, virConnectCloseReason);  -- /usr/include/libvirt/libvirt-common.h:120

  --*
  -- * virTypedParameterType:
  -- *
  -- * Express the type of a virTypedParameter
  --  

  -- integer case  
  -- unsigned integer case  
  -- long long case  
  -- unsigned long long case  
  -- double case  
  -- boolean(character) case  
  -- string case  
   subtype virTypedParameterType is unsigned;
   VIR_TYPED_PARAM_INT : constant virTypedParameterType := 1;
   VIR_TYPED_PARAM_UINT : constant virTypedParameterType := 2;
   VIR_TYPED_PARAM_LLONG : constant virTypedParameterType := 3;
   VIR_TYPED_PARAM_ULLONG : constant virTypedParameterType := 4;
   VIR_TYPED_PARAM_DOUBLE : constant virTypedParameterType := 5;
   VIR_TYPED_PARAM_BOOLEAN : constant virTypedParameterType := 6;
   VIR_TYPED_PARAM_STRING : constant virTypedParameterType := 7;  -- /usr/include/libvirt/libvirt-common.h:139

  --*
  -- * virTypedParameterFlags:
  -- *
  -- * Flags related to libvirt APIs that use virTypedParameter.
  -- *
  -- * These enums should not conflict with those of virDomainModificationImpact.
  --  

  -- 1 << 0 is reserved for virDomainModificationImpact  
  -- 1 << 1 is reserved for virDomainModificationImpact  
  -- Older servers lacked the ability to handle string typed
  --     * parameters.  Attempts to set a string parameter with an older
  --     * server will fail at the client, but attempts to retrieve
  --     * parameters must not return strings from a new server to an
  --     * older client, so this flag exists to identify newer clients to
  --     * newer servers.  This flag is automatically set when needed, so
  --     * the user does not have to worry about it; however, manually
  --     * setting the flag can be used to reject servers that cannot
  --     * return typed strings, even if no strings would be returned.
  --      

   subtype virTypedParameterFlags is unsigned;
   VIR_TYPED_PARAM_STRING_OKAY : constant virTypedParameterFlags := 4;  -- /usr/include/libvirt/libvirt-common.h:164

  --*
  -- * VIR_TYPED_PARAM_FIELD_LENGTH:
  -- *
  -- * Macro providing the field length of virTypedParameter name
  --  

  --*
  -- * virTypedParameter:
  -- *
  -- * A named parameter, including a type and value.
  -- *
  -- * The types virSchedParameter, virBlkioParameter, and
  -- * virMemoryParameter are aliases of this type, for use when
  -- * targeting libvirt earlier than 0.9.2.
  --  

   type u_virTypedParameter;
   type anon_16 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            i : aliased int;  -- /usr/include/libvirt/libvirt-common.h:188
         when 1 =>
            ui : aliased unsigned;  -- /usr/include/libvirt/libvirt-common.h:189
         when 2 =>
            l : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-common.h:190
         when 3 =>
            ul : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-common.h:191
         when 4 =>
            d : aliased double;  -- /usr/include/libvirt/libvirt-common.h:192
         when 5 =>
            b : aliased char;  -- /usr/include/libvirt/libvirt-common.h:193
         when others =>
            s : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-common.h:194
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_16);
   pragma Unchecked_Union (anon_16);
   subtype u_virTypedParameter_field_array is Interfaces.C.char_array (0 .. 79);
   subtype virTypedParameter is u_virTypedParameter;

  -- parameter name  
   type u_virTypedParameter is record
      field : aliased u_virTypedParameter_field_array;  -- /usr/include/libvirt/libvirt-common.h:185
      c_type : aliased int;  -- /usr/include/libvirt/libvirt-common.h:186
      value : aliased anon_16;  -- /usr/include/libvirt/libvirt-common.h:195
   end record;
   pragma Convention (C_Pass_By_Copy, u_virTypedParameter);  -- /usr/include/libvirt/libvirt-common.h:184

  -- parameter type, virTypedParameterType  
  -- type is INT  
  -- type is UINT  
  -- type is LLONG  
  -- type is ULLONG  
  -- type is DOUBLE  
  -- type is BOOLEAN  
  -- type is STRING, may not be NULL  
  -- parameter value  
  --*
  -- * virTypedParameterPtr:
  -- *
  -- * a pointer to a virTypedParameter structure.
  --  

   type virTypedParameterPtr is access all virTypedParameter;  -- /usr/include/libvirt/libvirt-common.h:203

   function virTypedParamsGet
     (params : virTypedParameterPtr;
      nparams : int;
      name : Interfaces.C.Strings.chars_ptr) return virTypedParameterPtr;  -- /usr/include/libvirt/libvirt-common.h:205
   pragma Import (C, virTypedParamsGet, "virTypedParamsGet");

   function virTypedParamsGetInt
     (params : virTypedParameterPtr;
      nparams : int;
      name : Interfaces.C.Strings.chars_ptr;
      value : access int) return int;  -- /usr/include/libvirt/libvirt-common.h:208
   pragma Import (C, virTypedParamsGetInt, "virTypedParamsGetInt");

   function virTypedParamsGetUInt
     (params : virTypedParameterPtr;
      nparams : int;
      name : Interfaces.C.Strings.chars_ptr;
      value : access unsigned) return int;  -- /usr/include/libvirt/libvirt-common.h:212
   pragma Import (C, virTypedParamsGetUInt, "virTypedParamsGetUInt");

   function virTypedParamsGetLLong
     (params : virTypedParameterPtr;
      nparams : int;
      name : Interfaces.C.Strings.chars_ptr;
      value : access Long_Long_Integer) return int;  -- /usr/include/libvirt/libvirt-common.h:216
   pragma Import (C, virTypedParamsGetLLong, "virTypedParamsGetLLong");

   function virTypedParamsGetULLong
     (params : virTypedParameterPtr;
      nparams : int;
      name : Interfaces.C.Strings.chars_ptr;
      value : access Extensions.unsigned_long_long) return int;  -- /usr/include/libvirt/libvirt-common.h:220
   pragma Import (C, virTypedParamsGetULLong, "virTypedParamsGetULLong");

   function virTypedParamsGetDouble
     (params : virTypedParameterPtr;
      nparams : int;
      name : Interfaces.C.Strings.chars_ptr;
      value : access double) return int;  -- /usr/include/libvirt/libvirt-common.h:224
   pragma Import (C, virTypedParamsGetDouble, "virTypedParamsGetDouble");

   function virTypedParamsGetBoolean
     (params : virTypedParameterPtr;
      nparams : int;
      name : Interfaces.C.Strings.chars_ptr;
      value : access int) return int;  -- /usr/include/libvirt/libvirt-common.h:228
   pragma Import (C, virTypedParamsGetBoolean, "virTypedParamsGetBoolean");

   function virTypedParamsGetString
     (params : virTypedParameterPtr;
      nparams : int;
      name : Interfaces.C.Strings.chars_ptr;
      value : System.Address) return int;  -- /usr/include/libvirt/libvirt-common.h:232
   pragma Import (C, virTypedParamsGetString, "virTypedParamsGetString");

   function virTypedParamsAddInt
     (params : System.Address;
      nparams : access int;
      maxparams : access int;
      name : Interfaces.C.Strings.chars_ptr;
      value : int) return int;  -- /usr/include/libvirt/libvirt-common.h:237
   pragma Import (C, virTypedParamsAddInt, "virTypedParamsAddInt");

   function virTypedParamsAddUInt
     (params : System.Address;
      nparams : access int;
      maxparams : access int;
      name : Interfaces.C.Strings.chars_ptr;
      value : unsigned) return int;  -- /usr/include/libvirt/libvirt-common.h:242
   pragma Import (C, virTypedParamsAddUInt, "virTypedParamsAddUInt");

   function virTypedParamsAddLLong
     (params : System.Address;
      nparams : access int;
      maxparams : access int;
      name : Interfaces.C.Strings.chars_ptr;
      value : Long_Long_Integer) return int;  -- /usr/include/libvirt/libvirt-common.h:247
   pragma Import (C, virTypedParamsAddLLong, "virTypedParamsAddLLong");

   function virTypedParamsAddULLong
     (params : System.Address;
      nparams : access int;
      maxparams : access int;
      name : Interfaces.C.Strings.chars_ptr;
      value : Extensions.unsigned_long_long) return int;  -- /usr/include/libvirt/libvirt-common.h:252
   pragma Import (C, virTypedParamsAddULLong, "virTypedParamsAddULLong");

   function virTypedParamsAddDouble
     (params : System.Address;
      nparams : access int;
      maxparams : access int;
      name : Interfaces.C.Strings.chars_ptr;
      value : double) return int;  -- /usr/include/libvirt/libvirt-common.h:257
   pragma Import (C, virTypedParamsAddDouble, "virTypedParamsAddDouble");

   function virTypedParamsAddBoolean
     (params : System.Address;
      nparams : access int;
      maxparams : access int;
      name : Interfaces.C.Strings.chars_ptr;
      value : int) return int;  -- /usr/include/libvirt/libvirt-common.h:262
   pragma Import (C, virTypedParamsAddBoolean, "virTypedParamsAddBoolean");

   function virTypedParamsAddString
     (params : System.Address;
      nparams : access int;
      maxparams : access int;
      name : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-common.h:267
   pragma Import (C, virTypedParamsAddString, "virTypedParamsAddString");

   function virTypedParamsAddStringList
     (params : System.Address;
      nparams : access int;
      maxparams : access int;
      name : Interfaces.C.Strings.chars_ptr;
      values : System.Address) return int;  -- /usr/include/libvirt/libvirt-common.h:272
   pragma Import (C, virTypedParamsAddStringList, "virTypedParamsAddStringList");

   function virTypedParamsAddFromString
     (params : System.Address;
      nparams : access int;
      maxparams : access int;
      name : Interfaces.C.Strings.chars_ptr;
      c_type : int;
      value : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-common.h:277
   pragma Import (C, virTypedParamsAddFromString, "virTypedParamsAddFromString");

   procedure virTypedParamsClear (params : virTypedParameterPtr; nparams : int);  -- /usr/include/libvirt/libvirt-common.h:284
   pragma Import (C, virTypedParamsClear, "virTypedParamsClear");

   procedure virTypedParamsFree (params : virTypedParameterPtr; nparams : int);  -- /usr/include/libvirt/libvirt-common.h:286
   pragma Import (C, virTypedParamsFree, "virTypedParamsFree");

end Libvirt_Common_Api;
