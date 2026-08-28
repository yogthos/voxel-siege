// SPDX-License-Identifier: MIT
// Pointer/scalar-only shim over Box3D (github.com/erincatto/box3d) for jolt.ffi.
//
// jolt's FFI cannot express Box3D's by-value struct traffic (b3Vec3/b3Quat are
// float HFAs passed in FP registers; b3BodyId/b3WorldId are int composites), so
// every function here takes plain numbers or out-pointers and does the struct
// work in C. Ids cross the boundary as the integers b3StoreBodyId/b3StoreWorldId
// produce. See scripts/build-native.sh for the build.
#include <stdint.h>

#include <box3d/box3d.h>

uint32_t vb3_world_create( double gx, double gy, double gz, int worker_count )
{
	b3WorldDef def = b3DefaultWorldDef();
	def.gravity.x = (float)gx;
	def.gravity.y = (float)gy;
	def.gravity.z = (float)gz;
	def.workerCount = (uint32_t)worker_count;
	return b3StoreWorldId( b3CreateWorld( &def ) );
}

void vb3_world_destroy( uint32_t world )
{
	b3DestroyWorld( b3LoadWorldId( world ) );
}

void vb3_world_step( uint32_t world, float dt, int substeps )
{
	b3World_Step( b3LoadWorldId( world ), dt, substeps );
}

void vb3_world_explode( uint32_t world, double x, double y, double z, float radius, float falloff,
						float impulse_per_area )
{
	b3ExplosionDef def = b3DefaultExplosionDef();
	def.position.x = (float)x;
	def.position.y = (float)y;
	def.position.z = (float)z;
	def.radius = radius;
	def.falloff = falloff;
	def.impulsePerArea = impulse_per_area;
	b3World_Explode( b3LoadWorldId( world ), &def );
}

uint64_t vb3_body_create( uint32_t world, int type, double x, double y, double z, double qx, double qy,
						  double qz, double qw, int awake )
{
	b3BodyDef def = b3DefaultBodyDef();
	def.type = (b3BodyType)type;
	def.position.x = (float)x;
	def.position.y = (float)y;
	def.position.z = (float)z;
	def.rotation.v = (b3Vec3){ (float)qx, (float)qy, (float)qz };
	def.rotation.s = (float)qw;
	def.isAwake = awake != 0;
	// heavy-masonry damping: bleeds micro-rotation so rubble reaches sleep
	// instead of skittering against its neighbours forever
	def.linearDamping = 0.6f;
	def.angularDamping = 0.9f;
	// rubble drifting under ~0.75 m/s is settling stone, not motion - let it
	// sleep (Box3D default 0.05 keeps nudged piles awake forever, and one
	// awake body in a pile re-wakes everything it touches)
	def.sleepThreshold = 0.75f;
	return b3StoreBodyId( b3CreateBody( b3LoadWorldId( world ), &def ) );
}

void vb3_body_destroy( uint64_t body )
{
	if ( body != 0 )
	{
		b3DestroyBody( b3LoadBodyId( body ) );
	}
}

void vb3_body_add_box( uint64_t body, double lx, double ly, double lz, float hx, float hy, float hz,
					   float density, float friction, float restitution )
{
	b3ShapeDef sd = b3DefaultShapeDef();
	sd.density = density;
	sd.baseMaterial.friction = friction;
	sd.baseMaterial.restitution = restitution;
	b3BoxHull hull = b3MakeOffsetBoxHull( hx, hy, hz, (b3Vec3){ (float)lx, (float)ly, (float)lz } );
	b3CreateHullShape( b3LoadBodyId( body ), &sd, &hull.base );
}

uint64_t vb3_ball_create( uint32_t world, double x, double y, double z, float radius, float vx, float vy,
						  float vz, float density, float friction, float restitution )
{
	b3BodyDef def = b3DefaultBodyDef();
	def.type = b3_dynamicBody;
	def.position.x = (float)x;
	def.position.y = (float)y;
	def.position.z = (float)z;
	def.linearVelocity.x = vx;
	def.linearVelocity.y = vy;
	def.linearVelocity.z = vz;

	b3BodyId body = b3CreateBody( b3LoadWorldId( world ), &def );

	b3ShapeDef sd = b3DefaultShapeDef();
	sd.density = density;
	sd.baseMaterial.friction = friction;
	sd.baseMaterial.restitution = restitution;
	b3Sphere sphere = { { 0.0f, 0.0f, 0.0f }, radius };
	b3CreateSphereShape( body, &sd, &sphere );

	return b3StoreBodyId( body );
}

void vb3_body_transform( uint64_t body, double* pos_out, float* quat_out )
{
	b3WorldTransform t = b3Body_GetTransform( b3LoadBodyId( body ) );
	pos_out[0] = t.p.x;
	pos_out[1] = t.p.y;
	pos_out[2] = t.p.z;
	quat_out[0] = t.q.v.x;
	quat_out[1] = t.q.v.y;
	quat_out[2] = t.q.v.z;
	quat_out[3] = t.q.s;
}

void vb3_body_velocity( uint64_t body, float* vel_out )
{
	b3Vec3 v = b3Body_GetLinearVelocity( b3LoadBodyId( body ) );
	vel_out[0] = v.x;
	vel_out[1] = v.y;
	vel_out[2] = v.z;
}

int vb3_body_awake( uint64_t body )
{
	return b3Body_IsAwake( b3LoadBodyId( body ) ) ? 1 : 0;
}

void vb3_body_set_velocity( uint64_t body, float vx, float vy, float vz )
{
	b3Body_SetLinearVelocity( b3LoadBodyId( body ), (b3Vec3){ vx, vy, vz } );
}

void vb3_body_set_awake( uint64_t body, int awake )
{
	// sleeping through the API settles the body's whole touching island
	b3Body_SetAwake( b3LoadBodyId( body ), awake != 0 );
}
