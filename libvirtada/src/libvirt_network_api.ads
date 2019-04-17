pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Libvirt_Host_Api; use Libvirt_Host_Api;
with Interfaces.C.Strings;
with Libvirt_Common_Api; use Libvirt_Common_Api;

package Libvirt_Network_Api is
   pragma Preelaborate;

   --  arg-macro: function VIR_NETWORK_EVENT_CALLBACK (cb)
   --    return (virConnectNetworkEventGenericCallback)(cb);
  -- * libvirt-network.h
  -- * Summary: APIs for management of networks
  -- * Description: Provides APIs for the management of networks
  -- * Author: Daniel Veillard <veillard@redhat.com>
  -- *
  -- * Copyright (C) 2006-2014 Red Hat, Inc.
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

  -- dump inactive network information  
   subtype virNetworkXMLFlags is unsigned;
   VIR_NETWORK_XML_INACTIVE : constant virNetworkXMLFlags := 1;  -- /usr/include/libvirt/libvirt-network.h:33

  --*
  -- * virNetwork:
  -- *
  -- * a virNetwork is a private structure representing a virtual network.
  --  

   --  skipped empty struct u_virNetwork

   --  skipped empty struct virNetwork

  --*
  -- * virNetworkPtr:
  -- *
  -- * a virNetworkPtr is pointer to a virNetwork private structure, this is the
  -- * type used to reference a virtual network in the API.
  --  

   type virNetworkPtr is new System.Address;  -- /usr/include/libvirt/libvirt-network.h:48

  -- * Get connection from network.
  --  

   function virNetworkGetConnect (network : virNetworkPtr) return virConnectPtr;  -- /usr/include/libvirt/libvirt-network.h:53
   pragma Import (C, virNetworkGetConnect, "virNetworkGetConnect");

  -- * List active networks
  --  

   function virConnectNumOfNetworks (conn : virConnectPtr) return int;  -- /usr/include/libvirt/libvirt-network.h:58
   pragma Import (C, virConnectNumOfNetworks, "virConnectNumOfNetworks");

   function virConnectListNetworks
     (conn : virConnectPtr;
      names : System.Address;
      maxnames : int) return int;  -- /usr/include/libvirt/libvirt-network.h:59
   pragma Import (C, virConnectListNetworks, "virConnectListNetworks");

  -- * List inactive networks
  --  

   function virConnectNumOfDefinedNetworks (conn : virConnectPtr) return int;  -- /usr/include/libvirt/libvirt-network.h:66
   pragma Import (C, virConnectNumOfDefinedNetworks, "virConnectNumOfDefinedNetworks");

   function virConnectListDefinedNetworks
     (conn : virConnectPtr;
      names : System.Address;
      maxnames : int) return int;  -- /usr/include/libvirt/libvirt-network.h:67
   pragma Import (C, virConnectListDefinedNetworks, "virConnectListDefinedNetworks");

  -- * virConnectListAllNetworks:
  -- *
  -- * Flags used to filter the returned networks. Flags in each group
  -- * are exclusive attributes of a network.
  --  

   subtype virConnectListAllNetworksFlags is unsigned;
   VIR_CONNECT_LIST_NETWORKS_INACTIVE : constant virConnectListAllNetworksFlags := 1;
   VIR_CONNECT_LIST_NETWORKS_ACTIVE : constant virConnectListAllNetworksFlags := 2;
   VIR_CONNECT_LIST_NETWORKS_PERSISTENT : constant virConnectListAllNetworksFlags := 4;
   VIR_CONNECT_LIST_NETWORKS_TRANSIENT : constant virConnectListAllNetworksFlags := 8;
   VIR_CONNECT_LIST_NETWORKS_AUTOSTART : constant virConnectListAllNetworksFlags := 16;
   VIR_CONNECT_LIST_NETWORKS_NO_AUTOSTART : constant virConnectListAllNetworksFlags := 32;  -- /usr/include/libvirt/libvirt-network.h:85

   function virConnectListAllNetworks
     (conn : virConnectPtr;
      nets : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-network.h:87
   pragma Import (C, virConnectListAllNetworks, "virConnectListAllNetworks");

  -- * Lookup network by name or uuid
  --  

   function virNetworkLookupByName (conn : virConnectPtr; name : Interfaces.C.Strings.chars_ptr) return virNetworkPtr;  -- /usr/include/libvirt/libvirt-network.h:94
   pragma Import (C, virNetworkLookupByName, "virNetworkLookupByName");

   function virNetworkLookupByUUID (conn : virConnectPtr; uuid : access unsigned_char) return virNetworkPtr;  -- /usr/include/libvirt/libvirt-network.h:96
   pragma Import (C, virNetworkLookupByUUID, "virNetworkLookupByUUID");

   function virNetworkLookupByUUIDString (conn : virConnectPtr; uuid : Interfaces.C.Strings.chars_ptr) return virNetworkPtr;  -- /usr/include/libvirt/libvirt-network.h:98
   pragma Import (C, virNetworkLookupByUUIDString, "virNetworkLookupByUUIDString");

  -- * Create active transient network
  --  

   function virNetworkCreateXML (conn : virConnectPtr; xmlDesc : Interfaces.C.Strings.chars_ptr) return virNetworkPtr;  -- /usr/include/libvirt/libvirt-network.h:104
   pragma Import (C, virNetworkCreateXML, "virNetworkCreateXML");

  -- * Define inactive persistent network
  --  

   function virNetworkDefineXML (conn : virConnectPtr; xmlDesc : Interfaces.C.Strings.chars_ptr) return virNetworkPtr;  -- /usr/include/libvirt/libvirt-network.h:110
   pragma Import (C, virNetworkDefineXML, "virNetworkDefineXML");

  -- * Delete persistent network
  --  

   function virNetworkUndefine (network : virNetworkPtr) return int;  -- /usr/include/libvirt/libvirt-network.h:116
   pragma Import (C, virNetworkUndefine, "virNetworkUndefine");

  --*
  -- * virNetworkUpdateCommand:
  -- *
  -- * describes which type of update to perform on a <network>
  -- * definition.
  -- *
  --  

  -- (invalid)  
  -- modify an existing element  
  -- delete an existing element  
  -- add an element at end of list  
  -- add an element at start of list  
   type virNetworkUpdateCommand is 
     (VIR_NETWORK_UPDATE_COMMAND_NONE,
      VIR_NETWORK_UPDATE_COMMAND_MODIFY,
      VIR_NETWORK_UPDATE_COMMAND_DELETE,
      VIR_NETWORK_UPDATE_COMMAND_ADD_LAST,
      VIR_NETWORK_UPDATE_COMMAND_ADD_FIRST);
   pragma Convention (C, virNetworkUpdateCommand);  -- /usr/include/libvirt/libvirt-network.h:134

  --*
  -- * virNetworkUpdateSection:
  -- *
  -- * describes which section of a <network> definition the provided
  -- * xml should be applied to.
  -- *
  --  

  -- (invalid)  
  -- <bridge>  
  -- <domain>  
  -- <ip>  
  -- <ip>/<dhcp>/<host>  
  -- <ip>/<dhcp>/<range>  
  -- <forward>  
  -- <forward>/<interface>  
  -- <forward>/<pf>  
  -- <portgroup>  
  -- <dns>/<host>  
  -- <dns>/<txt>  
  -- <dns>/<srv>  
   type virNetworkUpdateSection is 
     (VIR_NETWORK_SECTION_NONE,
      VIR_NETWORK_SECTION_BRIDGE,
      VIR_NETWORK_SECTION_DOMAIN,
      VIR_NETWORK_SECTION_IP,
      VIR_NETWORK_SECTION_IP_DHCP_HOST,
      VIR_NETWORK_SECTION_IP_DHCP_RANGE,
      VIR_NETWORK_SECTION_FORWARD,
      VIR_NETWORK_SECTION_FORWARD_INTERFACE,
      VIR_NETWORK_SECTION_FORWARD_PF,
      VIR_NETWORK_SECTION_PORTGROUP,
      VIR_NETWORK_SECTION_DNS_HOST,
      VIR_NETWORK_SECTION_DNS_TXT,
      VIR_NETWORK_SECTION_DNS_SRV);
   pragma Convention (C, virNetworkUpdateSection);  -- /usr/include/libvirt/libvirt-network.h:160

  --*
  -- * virNetworkUpdateFlags:
  -- *
  -- * Flags to control options for virNetworkUpdate()
  --  

  -- affect live if network is active,
  --                                                   config if it's not active  

  -- affect live state of network only  
  -- affect persistent config only  
   type virNetworkUpdateFlags is 
     (VIR_NETWORK_UPDATE_AFFECT_CURRENT,
      VIR_NETWORK_UPDATE_AFFECT_LIVE,
      VIR_NETWORK_UPDATE_AFFECT_CONFIG);
   pragma Convention (C, virNetworkUpdateFlags);  -- /usr/include/libvirt/libvirt-network.h:172

  -- * Update an existing network definition
  --  

   function virNetworkUpdate
     (network : virNetworkPtr;
      command : unsigned;
      section : unsigned;
      parentIndex : int;
      xml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-network.h:177
   pragma Import (C, virNetworkUpdate, "virNetworkUpdate");

  -- virNetworkUpdateCommand  
  -- virNetworkUpdateSection  
  -- * Activate persistent network
  --  

   function virNetworkCreate (network : virNetworkPtr) return int;  -- /usr/include/libvirt/libvirt-network.h:187
   pragma Import (C, virNetworkCreate, "virNetworkCreate");

  -- * Network destroy/free
  --  

   function virNetworkDestroy (network : virNetworkPtr) return int;  -- /usr/include/libvirt/libvirt-network.h:192
   pragma Import (C, virNetworkDestroy, "virNetworkDestroy");

   function virNetworkRef (network : virNetworkPtr) return int;  -- /usr/include/libvirt/libvirt-network.h:193
   pragma Import (C, virNetworkRef, "virNetworkRef");

   function virNetworkFree (network : virNetworkPtr) return int;  -- /usr/include/libvirt/libvirt-network.h:194
   pragma Import (C, virNetworkFree, "virNetworkFree");

  -- * Network information
  --  

   function virNetworkGetName (network : virNetworkPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-network.h:199
   pragma Import (C, virNetworkGetName, "virNetworkGetName");

   function virNetworkGetUUID (network : virNetworkPtr; uuid : access unsigned_char) return int;  -- /usr/include/libvirt/libvirt-network.h:200
   pragma Import (C, virNetworkGetUUID, "virNetworkGetUUID");

   function virNetworkGetUUIDString (network : virNetworkPtr; buf : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-network.h:202
   pragma Import (C, virNetworkGetUUIDString, "virNetworkGetUUIDString");

   function virNetworkGetXMLDesc (network : virNetworkPtr; flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-network.h:204
   pragma Import (C, virNetworkGetXMLDesc, "virNetworkGetXMLDesc");

   function virNetworkGetBridgeName (network : virNetworkPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-network.h:206
   pragma Import (C, virNetworkGetBridgeName, "virNetworkGetBridgeName");

   function virNetworkGetAutostart (network : virNetworkPtr; autostart : access int) return int;  -- /usr/include/libvirt/libvirt-network.h:208
   pragma Import (C, virNetworkGetAutostart, "virNetworkGetAutostart");

   function virNetworkSetAutostart (network : virNetworkPtr; autostart : int) return int;  -- /usr/include/libvirt/libvirt-network.h:210
   pragma Import (C, virNetworkSetAutostart, "virNetworkSetAutostart");

   function virNetworkIsActive (net : virNetworkPtr) return int;  -- /usr/include/libvirt/libvirt-network.h:213
   pragma Import (C, virNetworkIsActive, "virNetworkIsActive");

   function virNetworkIsPersistent (net : virNetworkPtr) return int;  -- /usr/include/libvirt/libvirt-network.h:214
   pragma Import (C, virNetworkIsPersistent, "virNetworkIsPersistent");

  --*
  -- * virNetworkEventLifecycleType:
  -- *
  -- * a virNetworkEventLifecycleType is emitted during network lifecycle events
  --  

   type virNetworkEventLifecycleType is 
     (VIR_NETWORK_EVENT_DEFINED,
      VIR_NETWORK_EVENT_UNDEFINED,
      VIR_NETWORK_EVENT_STARTED,
      VIR_NETWORK_EVENT_STOPPED);
   pragma Convention (C, virNetworkEventLifecycleType);  -- /usr/include/libvirt/libvirt-network.h:230

  --*
  -- * virConnectNetworkEventLifecycleCallback:
  -- * @conn: connection object
  -- * @net: network on which the event occurred
  -- * @event: The specific virNetworkEventLifeCycleType which occurred
  -- * @detail: contains some details on the reason of the event.
  -- *          It will be 0 for the while.
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when the network is started or stopped.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_NETWORK_EVENT_ID_LIFECYCLE with virConnectNetworkEventRegisterAny()
  --  

   type virConnectNetworkEventLifecycleCallback is access procedure
        (conn   : virConnectPtr;
         net    : virNetworkPtr;
         event  : int;
         detail : int;
         opaque : System.Address);
   pragma Convention (C, virConnectNetworkEventLifecycleCallback);  -- /usr/include/libvirt/libvirt-network.h:246

  --*
  -- * VIR_NETWORK_EVENT_CALLBACK:
  -- *
  -- * Used to cast the event specific callback into the generic one
  -- * for use for virConnectNetworkEventRegisterAny()
  --  

  --*
  -- * virNetworkEventID:
  -- *
  -- * An enumeration of supported eventId parameters for
  -- * virConnectNetworkEventRegisterAny().  Each event id determines which
  -- * signature of callback function will be used.
  --  

  -- virConnectNetworkEventLifecycleCallback  
  --     * NB: this enum value will increase over time as new events are
  --     * added to the libvirt API. It reflects the last event ID supported
  --     * by this version of the libvirt API.
  --      

   type virNetworkEventID is 
     (VIR_NETWORK_EVENT_ID_LIFECYCLE);
   pragma Convention (C, virNetworkEventID);  -- /usr/include/libvirt/libvirt-network.h:278

   type virIPAddrType is 
     (VIR_IP_ADDR_TYPE_IPV4,
      VIR_IP_ADDR_TYPE_IPV6);
   pragma Convention (C, virIPAddrType);  -- /usr/include/libvirt/libvirt-network.h:287

   type u_virNetworkDHCPLease;
   subtype virNetworkDHCPLease is u_virNetworkDHCPLease;

   type virNetworkDHCPLeasePtr is new System.Address;  -- /usr/include/libvirt/libvirt-network.h:290

  -- Network interface name  
   type u_virNetworkDHCPLease is record
      iface : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-network.h:292
      expirytime : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-network.h:293
      c_type : aliased int;  -- /usr/include/libvirt/libvirt-network.h:294
      mac : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-network.h:295
      iaid : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-network.h:296
      ipaddr : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-network.h:297
      prefix : aliased unsigned;  -- /usr/include/libvirt/libvirt-network.h:298
      hostname : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-network.h:299
      clientid : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-network.h:300
   end record;
   pragma Convention (C_Pass_By_Copy, u_virNetworkDHCPLease);  -- /usr/include/libvirt/libvirt-network.h:291

  -- Seconds since epoch  
  -- virIPAddrType  
  -- MAC address  
  -- IAID  
  -- IP address  
  -- IP address prefix  
  -- Hostname  
  -- Client ID or DUID  
   procedure virNetworkDHCPLeaseFree (lease : virNetworkDHCPLeasePtr);  -- /usr/include/libvirt/libvirt-network.h:303
   pragma Import (C, virNetworkDHCPLeaseFree, "virNetworkDHCPLeaseFree");

   function virNetworkGetDHCPLeases
     (network : virNetworkPtr;
      mac : Interfaces.C.Strings.chars_ptr;
      leases : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-network.h:305
   pragma Import (C, virNetworkGetDHCPLeases, "virNetworkGetDHCPLeases");

  --*
  -- * virConnectNetworkEventGenericCallback:
  -- * @conn: the connection pointer
  -- * @net: the network pointer
  -- * @opaque: application specified data
  -- *
  -- * A generic network event callback handler, for use with
  -- * virConnectNetworkEventRegisterAny(). Specific events usually
  -- * have a customization with extra parameters, often with @opaque being
  -- * passed in a different parameter position; use VIR_NETWORK_EVENT_CALLBACK()
  -- * when registering an appropriate handler.
  --  

   type virConnectNetworkEventGenericCallback is access procedure
        (conn   : virConnectPtr;
         net    : virNetworkPtr;
         opaque : System.Address);
   pragma Convention (C, virConnectNetworkEventGenericCallback);  -- /usr/include/libvirt/libvirt-network.h:322

  -- Use VIR_NETWORK_EVENT_CALLBACK() to cast the 'cb' parameter   
   function virConnectNetworkEventRegisterAny
     (conn : virConnectPtr;
      net : virNetworkPtr;
      eventID : int;
      cb : virConnectNetworkEventGenericCallback;
      opaque : System.Address;
      freecb : virFreeCallback) return int;  -- /usr/include/libvirt/libvirt-network.h:327
   pragma Import (C, virConnectNetworkEventRegisterAny, "virConnectNetworkEventRegisterAny");

  -- Optional, to filter  
   function virConnectNetworkEventDeregisterAny (conn : virConnectPtr; callbackID : int) return int;  -- /usr/include/libvirt/libvirt-network.h:334
   pragma Import (C, virConnectNetworkEventDeregisterAny, "virConnectNetworkEventDeregisterAny");

end Libvirt_Network_Api;
