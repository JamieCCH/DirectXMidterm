//***************************************************************************************
// LitColumnsApp.cpp by Frank Luna (C) 2015 All Rights Reserved.
//***************************************************************************************

#include "../../Common/d3dApp.h"
#include "../../Common/MathHelper.h"
#include "../../Common/UploadBuffer.h"
#include "../../Common/GeometryGenerator.h"
#include "FrameResource.h"
#include <stdlib.h>
#include <time.h>
#include <iostream>
#include <random>

using Microsoft::WRL::ComPtr;
using namespace DirectX;
using namespace DirectX::PackedVector;

#pragma comment(lib, "d3dcompiler.lib")
#pragma comment(lib, "D3D12.lib")

const int gNumFrameResources = 3;

// Lightweight structure stores parameters to draw a shape.  This will
// vary from app-to-app.
struct RenderItem
{
	RenderItem() = default;

    // World matrix of the shape that describes the object's local space
    // relative to the world space, which defines the position, orientation,
    // and scale of the object in the world.
    XMFLOAT4X4 World = MathHelper::Identity4x4();

	XMFLOAT4X4 TexTransform = MathHelper::Identity4x4();

	// Dirty flag indicating the object data has changed and we need to update the constant buffer.
	// Because we have an object cbuffer for each FrameResource, we have to apply the
	// update to each FrameResource.  Thus, when we modify obect data we should set 
	// NumFramesDirty = gNumFrameResources so that each frame resource gets the update.
	int NumFramesDirty = gNumFrameResources;

	// Index into GPU constant buffer corresponding to the ObjectCB for this render item.
	UINT ObjCBIndex = -1;

	Material* Mat = nullptr;
	MeshGeometry* Geo = nullptr;

    // Primitive topology.
    D3D12_PRIMITIVE_TOPOLOGY PrimitiveType = D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST;

    // DrawIndexedInstanced parameters.
    UINT IndexCount = 0;
    UINT StartIndexLocation = 0;
    int BaseVertexLocation = 0;
};

class LitColumnsApp : public D3DApp
{
public:
    LitColumnsApp(HINSTANCE hInstance);
    LitColumnsApp(const LitColumnsApp& rhs) = delete;
    LitColumnsApp& operator=(const LitColumnsApp& rhs) = delete;
    ~LitColumnsApp();

    virtual bool Initialize()override;

private:
    virtual void OnResize()override;
    virtual void Update(const GameTimer& gt)override;
    virtual void Draw(const GameTimer& gt)override;

    virtual void OnMouseDown(WPARAM btnState, int x, int y)override;
    virtual void OnMouseUp(WPARAM btnState, int x, int y)override;
    virtual void OnMouseMove(WPARAM btnState, int x, int y)override;

    void OnKeyboardInput(const GameTimer& gt);
	void UpdateCamera(const GameTimer& gt);
	void AnimateMaterials(const GameTimer& gt);
	void UpdateObjectCBs(const GameTimer& gt);
	void UpdateMaterialCBs(const GameTimer& gt);
	void UpdateMainPassCB(const GameTimer& gt);

    void BuildRootSignature();
    void BuildShadersAndInputLayout();
    void BuildShapeGeometry();
	//void BuildSkullGeometry();
    void BuildPSOs();
    void BuildFrameResources();
    void BuildMaterials();
    void BuildRenderItems();
    void DrawRenderItems(ID3D12GraphicsCommandList* cmdList, const std::vector<RenderItem*>& ritems);
 
private:

    std::vector<std::unique_ptr<FrameResource>> mFrameResources;
    FrameResource* mCurrFrameResource = nullptr;
    int mCurrFrameResourceIndex = 0;

    UINT mCbvSrvDescriptorSize = 0;

    ComPtr<ID3D12RootSignature> mRootSignature = nullptr;

	ComPtr<ID3D12DescriptorHeap> mSrvDescriptorHeap = nullptr;

	std::unordered_map<std::string, std::unique_ptr<MeshGeometry>> mGeometries;
	std::unordered_map<std::string, std::unique_ptr<Material>> mMaterials;
	std::unordered_map<std::string, std::unique_ptr<Texture>> mTextures;
	std::unordered_map<std::string, ComPtr<ID3DBlob>> mShaders;

    std::vector<D3D12_INPUT_ELEMENT_DESC> mInputLayout;

    ComPtr<ID3D12PipelineState> mOpaquePSO = nullptr;
 
	// List of all the render items.
	std::vector<std::unique_ptr<RenderItem>> mAllRitems;

	// Render items divided by PSO.
	std::vector<RenderItem*> mOpaqueRitems;

    PassConstants mMainPassCB;

	XMFLOAT3 mEyePos = { 0.0f, 0.0f, 0.0f };
	XMFLOAT4X4 mView = MathHelper::Identity4x4();
	XMFLOAT4X4 mProj = MathHelper::Identity4x4();

	XMFLOAT4 ambientLightColor = XMFLOAT4(0.f,0.f,0.f,0.f);
	UINT pointIndex = 1;

    float mTheta = 1.5f*XM_PI;
    float mPhi = 0.2f*XM_PI;
    float mRadius = 15.0f;

    POINT mLastMousePos;
};

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE prevInstance,
    PSTR cmdLine, int showCmd)
{
    // Enable run-time memory check for debug builds.
#if defined(DEBUG) | defined(_DEBUG)
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif

    try
    {
        LitColumnsApp theApp(hInstance);
        if(!theApp.Initialize())
            return 0;

        return theApp.Run();
    }
    catch(DxException& e)
    {
        MessageBox(nullptr, e.ToString().c_str(), L"HR Failed", MB_OK);
        return 0;
    }
}

LitColumnsApp::LitColumnsApp(HINSTANCE hInstance)
    : D3DApp(hInstance)
{
}

LitColumnsApp::~LitColumnsApp()
{
    if(md3dDevice != nullptr)
        FlushCommandQueue();
}

bool LitColumnsApp::Initialize()
{
	srand((unsigned)time(NULL));

    if(!D3DApp::Initialize())
        return false;

    // Reset the command list to prep for initialization commands.
    ThrowIfFailed(mCommandList->Reset(mDirectCmdListAlloc.Get(), nullptr));

    // Get the increment size of a descriptor in this heap type.  This is hardware specific, 
	// so we have to query this information.
    mCbvSrvDescriptorSize = md3dDevice->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);

    BuildRootSignature();
    BuildShadersAndInputLayout();
    BuildShapeGeometry();
	//BuildSkullGeometry();
	BuildMaterials();
    BuildRenderItems();
    BuildFrameResources();
    BuildPSOs();

    // Execute the initialization commands.
    ThrowIfFailed(mCommandList->Close());
    ID3D12CommandList* cmdsLists[] = { mCommandList.Get() };
    mCommandQueue->ExecuteCommandLists(_countof(cmdsLists), cmdsLists);

    // Wait until initialization is complete.
    FlushCommandQueue();

    return true;
}
 
void LitColumnsApp::OnResize()
{
    D3DApp::OnResize();

    // The window resized, so update the aspect ratio and recompute the projection matrix.
    XMMATRIX P = XMMatrixPerspectiveFovLH(0.25f*MathHelper::Pi, AspectRatio(), 1.0f, 1000.0f);
    XMStoreFloat4x4(&mProj, P);
}

void LitColumnsApp::Update(const GameTimer& gt)
{
    OnKeyboardInput(gt);
	UpdateCamera(gt);

    // Cycle through the circular frame resource array.
    mCurrFrameResourceIndex = (mCurrFrameResourceIndex + 1) % gNumFrameResources;
    mCurrFrameResource = mFrameResources[mCurrFrameResourceIndex].get();

    // Has the GPU finished processing the commands of the current frame resource?
    // If not, wait until the GPU has completed commands up to this fence point.
    if(mCurrFrameResource->Fence != 0 && mFence->GetCompletedValue() < mCurrFrameResource->Fence)
    {
        HANDLE eventHandle = CreateEventEx(nullptr, false, false, EVENT_ALL_ACCESS);
        ThrowIfFailed(mFence->SetEventOnCompletion(mCurrFrameResource->Fence, eventHandle));
        WaitForSingleObject(eventHandle, INFINITE);
        CloseHandle(eventHandle);
    }

	AnimateMaterials(gt);
	UpdateObjectCBs(gt);
	UpdateMaterialCBs(gt);
	UpdateMainPassCB(gt);
}

void LitColumnsApp::Draw(const GameTimer& gt)
{
    auto cmdListAlloc = mCurrFrameResource->CmdListAlloc;

    // Reuse the memory associated with command recording.
    // We can only reset when the associated command lists have finished execution on the GPU.
    ThrowIfFailed(cmdListAlloc->Reset());

    // A command list can be reset after it has been added to the command queue via ExecuteCommandList.
    // Reusing the command list reuses memory.
    ThrowIfFailed(mCommandList->Reset(cmdListAlloc.Get(), mOpaquePSO.Get()));

    mCommandList->RSSetViewports(1, &mScreenViewport);
    mCommandList->RSSetScissorRects(1, &mScissorRect);

    // Indicate a state transition on the resource usage.
	mCommandList->ResourceBarrier(1, &CD3DX12_RESOURCE_BARRIER::Transition(CurrentBackBuffer(),
		D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET));

    // Clear the back buffer and depth buffer.
    mCommandList->ClearRenderTargetView(CurrentBackBufferView(), Colors::LightSteelBlue, 0, nullptr);
    mCommandList->ClearDepthStencilView(DepthStencilView(), D3D12_CLEAR_FLAG_DEPTH | D3D12_CLEAR_FLAG_STENCIL, 1.0f, 0, 0, nullptr);

    // Specify the buffers we are going to render to.
    mCommandList->OMSetRenderTargets(1, &CurrentBackBufferView(), true, &DepthStencilView());

	mCommandList->SetGraphicsRootSignature(mRootSignature.Get());

	auto passCB = mCurrFrameResource->PassCB->Resource();
	mCommandList->SetGraphicsRootConstantBufferView(2, passCB->GetGPUVirtualAddress());

    DrawRenderItems(mCommandList.Get(), mOpaqueRitems);

    // Indicate a state transition on the resource usage.
	mCommandList->ResourceBarrier(1, &CD3DX12_RESOURCE_BARRIER::Transition(CurrentBackBuffer(),
		D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT));

    // Done recording commands.
    ThrowIfFailed(mCommandList->Close());

    // Add the command list to the queue for execution.
    ID3D12CommandList* cmdsLists[] = { mCommandList.Get() };
    mCommandQueue->ExecuteCommandLists(_countof(cmdsLists), cmdsLists);

    // Swap the back and front buffers
    ThrowIfFailed(mSwapChain->Present(0, 0));
	mCurrBackBuffer = (mCurrBackBuffer + 1) % SwapChainBufferCount;

    // Advance the fence value to mark commands up to this fence point.
    mCurrFrameResource->Fence = ++mCurrentFence;

    // Add an instruction to the command queue to set a new fence point. 
    // Because we are on the GPU timeline, the new fence point won't be 
    // set until the GPU finishes processing all the commands prior to this Signal().
    mCommandQueue->Signal(mFence.Get(), mCurrentFence);
}

void LitColumnsApp::OnMouseDown(WPARAM btnState, int x, int y)
{
    mLastMousePos.x = x;
    mLastMousePos.y = y;

    SetCapture(mhMainWnd);
}

void LitColumnsApp::OnMouseUp(WPARAM btnState, int x, int y)
{
    ReleaseCapture();
}

void LitColumnsApp::OnMouseMove(WPARAM btnState, int x, int y)
{
    if((btnState & MK_LBUTTON) != 0)
    {
        // Make each pixel correspond to a quarter of a degree.
        float dx = XMConvertToRadians(0.25f*static_cast<float>(x - mLastMousePos.x));
        float dy = XMConvertToRadians(0.25f*static_cast<float>(y - mLastMousePos.y));

        // Update angles based on input to orbit camera around box.
        mTheta += dx;
        mPhi += dy;

        // Restrict the angle mPhi.
        mPhi = MathHelper::Clamp(mPhi, 0.1f, MathHelper::Pi - 0.1f);
    }
    else if((btnState & MK_RBUTTON) != 0)
    {
        // Make each pixel correspond to 0.2 unit in the scene.
        float dx = 0.05f*static_cast<float>(x - mLastMousePos.x);
        float dy = 0.05f*static_cast<float>(y - mLastMousePos.y);

        // Update the camera radius based on input.
        mRadius += dx - dy;

        // Restrict the radius.
        mRadius = MathHelper::Clamp(mRadius, 5.0f, 150.0f);
    }

    mLastMousePos.x = x;
    mLastMousePos.y = y;
}
 
void LitColumnsApp::OnKeyboardInput(const GameTimer& gt)
{
}
 
void LitColumnsApp::UpdateCamera(const GameTimer& gt)
{
	// Convert Spherical to Cartesian coordinates.
	mEyePos.x = mRadius*sinf(mPhi)*cosf(mTheta);
	mEyePos.z = mRadius*sinf(mPhi)*sinf(mTheta);
	mEyePos.y = mRadius*cosf(mPhi);

	// Build the view matrix.
	XMVECTOR pos = XMVectorSet(mEyePos.x, mEyePos.y, mEyePos.z, 1.0f);
	XMVECTOR target = XMVectorZero();
	XMVECTOR up = XMVectorSet(0.0f, 1.0f, 0.0f, 0.0f);

	XMMATRIX view = XMMatrixLookAtLH(pos, target, up);
	XMStoreFloat4x4(&mView, view);
}

void LitColumnsApp::AnimateMaterials(const GameTimer& gt)
{
	
}

void LitColumnsApp::UpdateObjectCBs(const GameTimer& gt)
{
	auto currObjectCB = mCurrFrameResource->ObjectCB.get();
	for(auto& e : mAllRitems)
	{
		// Only update the cbuffer data if the constants have changed.  
		// This needs to be tracked per frame resource.
		if(e->NumFramesDirty > 0)
		{
			XMMATRIX world = XMLoadFloat4x4(&e->World);
			XMMATRIX texTransform = XMLoadFloat4x4(&e->TexTransform);

			ObjectConstants objConstants;
			XMStoreFloat4x4(&objConstants.World, XMMatrixTranspose(world));
			XMStoreFloat4x4(&objConstants.TexTransform, XMMatrixTranspose(texTransform));

			currObjectCB->CopyData(e->ObjCBIndex, objConstants);

			// Next FrameResource need to be updated too.
			e->NumFramesDirty--;
		}
	}
}

void LitColumnsApp::UpdateMaterialCBs(const GameTimer& gt)
{
	auto currMaterialCB = mCurrFrameResource->MaterialCB.get();
	for(auto& e : mMaterials)
	{
		// Only update the cbuffer data if the constants have changed.  If the cbuffer
		// data changes, it needs to be updated for each FrameResource.
		Material* mat = e.second.get();
		if(mat->NumFramesDirty > 0)
		{
			XMMATRIX matTransform = XMLoadFloat4x4(&mat->MatTransform);

			MaterialConstants matConstants;
			matConstants.DiffuseAlbedo = mat->DiffuseAlbedo;
			matConstants.FresnelR0 = mat->FresnelR0;
			matConstants.Roughness = mat->Roughness;
			XMStoreFloat4x4(&matConstants.MatTransform, XMMatrixTranspose(matTransform));

			currMaterialCB->CopyData(mat->MatCBIndex, matConstants);

			// Next FrameResource need to be updated too.
			mat->NumFramesDirty--;
		}
	}
}

void LitColumnsApp::UpdateMainPassCB(const GameTimer& gt)
{
	XMMATRIX view = XMLoadFloat4x4(&mView);
	XMMATRIX proj = XMLoadFloat4x4(&mProj);

	XMMATRIX viewProj = XMMatrixMultiply(view, proj);
	XMMATRIX invView = XMMatrixInverse(&XMMatrixDeterminant(view), view);
	XMMATRIX invProj = XMMatrixInverse(&XMMatrixDeterminant(proj), proj);
	XMMATRIX invViewProj = XMMatrixInverse(&XMMatrixDeterminant(viewProj), viewProj);

	XMStoreFloat4x4(&mMainPassCB.View, XMMatrixTranspose(view));
	XMStoreFloat4x4(&mMainPassCB.InvView, XMMatrixTranspose(invView));
	XMStoreFloat4x4(&mMainPassCB.Proj, XMMatrixTranspose(proj));
	XMStoreFloat4x4(&mMainPassCB.InvProj, XMMatrixTranspose(invProj));
	XMStoreFloat4x4(&mMainPassCB.ViewProj, XMMatrixTranspose(viewProj));
	XMStoreFloat4x4(&mMainPassCB.InvViewProj, XMMatrixTranspose(invViewProj));
	mMainPassCB.EyePosW = mEyePos;
	mMainPassCB.RenderTargetSize = XMFLOAT2((float)mClientWidth, (float)mClientHeight);
	mMainPassCB.InvRenderTargetSize = XMFLOAT2(1.0f / mClientWidth, 1.0f / mClientHeight);
	mMainPassCB.NearZ = 1.0f;
	mMainPassCB.FarZ = 1000.0f;
	mMainPassCB.TotalTime = gt.TotalTime();
	mMainPassCB.DeltaTime = gt.DeltaTime();
	mMainPassCB.AmbientLight = ambientLightColor;

	auto currPassCB = mCurrFrameResource->PassCB.get();
	currPassCB->CopyData(0, mMainPassCB);
}

void LitColumnsApp::BuildRootSignature()
{
	// Root parameter can be a table, root descriptor or root constants.
	CD3DX12_ROOT_PARAMETER slotRootParameter[3];

	// Create root CBV.
	slotRootParameter[0].InitAsConstantBufferView(0);
	slotRootParameter[1].InitAsConstantBufferView(1);
	slotRootParameter[2].InitAsConstantBufferView(2);

	// A root signature is an array of root parameters.
	CD3DX12_ROOT_SIGNATURE_DESC rootSigDesc(3, slotRootParameter, 0, nullptr, D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT);

	// create a root signature with a single slot which points to a descriptor range consisting of a single constant buffer
	ComPtr<ID3DBlob> serializedRootSig = nullptr;
	ComPtr<ID3DBlob> errorBlob = nullptr;
	HRESULT hr = D3D12SerializeRootSignature(&rootSigDesc, D3D_ROOT_SIGNATURE_VERSION_1,
		serializedRootSig.GetAddressOf(), errorBlob.GetAddressOf());

	if(errorBlob != nullptr)
	{
		::OutputDebugStringA((char*)errorBlob->GetBufferPointer());
	}
	ThrowIfFailed(hr);

	ThrowIfFailed(md3dDevice->CreateRootSignature(
		0,
		serializedRootSig->GetBufferPointer(),
		serializedRootSig->GetBufferSize(),
		IID_PPV_ARGS(mRootSignature.GetAddressOf())));
}

void LitColumnsApp::BuildShadersAndInputLayout()
{
	const D3D_SHADER_MACRO alphaTestDefines[] =
	{
		"ALPHA_TEST", "1",
		NULL, NULL
	};

	mShaders["standardVS"] = d3dUtil::CompileShader(L"Shaders\\Default.hlsl", nullptr, "VS", "vs_5_1");
	mShaders["opaquePS"] = d3dUtil::CompileShader(L"Shaders\\Default.hlsl", nullptr, "PS", "ps_5_1");
	
    mInputLayout =
    {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
        { "NORMAL", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
		{ "TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT, 0, 24, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
    };
}

void LitColumnsApp::BuildShapeGeometry()
{
    GeometryGenerator geoGen;
	GeometryGenerator::MeshData candy = geoGen.CreateCandy(1.f, 2.f, 3);
	GeometryGenerator::MeshData squareBucket = geoGen.CreateSquareBucket(4.f, 2.f, 3.5f);
	GeometryGenerator::MeshData hexagonBucket = geoGen.CreateHexagonBucket(2.f, 2.f);

	//
	// We are concatenating all the geometry into one big vertex/index buffer.  So
	// define the regions in the buffer each submesh covers.
	//

	// Cache the vertex offsets to each object in the concatenated vertex buffer.
	UINT candyVertexOffset = 0;
	UINT squareBucketVertexOffset = (UINT)candy.Vertices.size();
	UINT hexagonBucketVertexOffset = squareBucketVertexOffset + (UINT)squareBucket.Vertices.size();

	// Cache the starting index for each object in the concatenated index buffer.
	UINT candyIndexOffset = 0;
	UINT squareBucketIndexOffset = (UINT)candy.Indices32.size();
	UINT hexagonBucketIndexOffset = squareBucketIndexOffset + (UINT)squareBucket.Indices32.size();

	SubmeshGeometry candySubmesh;
	candySubmesh.IndexCount = (UINT)candy.Indices32.size();
	candySubmesh.StartIndexLocation = candyIndexOffset;
	candySubmesh.BaseVertexLocation = candyVertexOffset;

	SubmeshGeometry squareBucketSubmesh;
	squareBucketSubmesh.IndexCount = (UINT)squareBucket.Indices32.size();
	squareBucketSubmesh.StartIndexLocation = squareBucketIndexOffset;
	squareBucketSubmesh.BaseVertexLocation = squareBucketVertexOffset;

	SubmeshGeometry hexagonBucketSubmesh;
	hexagonBucketSubmesh.IndexCount = (UINT)hexagonBucket.Indices32.size();
	hexagonBucketSubmesh.StartIndexLocation = hexagonBucketIndexOffset;
	hexagonBucketSubmesh.BaseVertexLocation = hexagonBucketVertexOffset;

	//
	// Extract the vertex elements we are interested in and pack the
	// vertices of all the meshes into one vertex buffer.
	//

	auto totalVertexCount =
		candy.Vertices.size() +
		squareBucket.Vertices.size() + 
		hexagonBucket.Vertices.size();

	std::vector<Vertex> vertices(totalVertexCount);

	UINT k = 0;
	for (size_t i = 0; i < candy.Vertices.size(); ++i, ++k) {
		vertices[k].Pos = candy.Vertices[i].Position;
		vertices[k].Normal = candy.Vertices[i].Normal;
	}
	for (size_t i = 0; i < squareBucket.Vertices.size(); ++i, ++k) {
		vertices[k].Pos = squareBucket.Vertices[i].Position;
		vertices[k].Normal = squareBucket.Vertices[i].Normal;
	}
	for (size_t i = 0; i < hexagonBucket.Vertices.size(); ++i, ++k) {
		vertices[k].Pos = hexagonBucket.Vertices[i].Position;
		vertices[k].Normal = hexagonBucket.Vertices[i].Normal;
	}

	std::vector<std::uint16_t> indices;
	indices.insert(indices.end(), std::begin(candy.GetIndices16()), std::end(candy.GetIndices16()));
	indices.insert(indices.end(), std::begin(squareBucket.GetIndices16()), std::end(squareBucket.GetIndices16()));
	indices.insert(indices.end(), std::begin(hexagonBucket.GetIndices16()), std::end(hexagonBucket.GetIndices16()));

    const UINT vbByteSize = (UINT)vertices.size() * sizeof(Vertex);
    const UINT ibByteSize = (UINT)indices.size()  * sizeof(std::uint16_t);

	auto geo = std::make_unique<MeshGeometry>();
	geo->Name = "shapeGeo";

	ThrowIfFailed(D3DCreateBlob(vbByteSize, &geo->VertexBufferCPU));
	CopyMemory(geo->VertexBufferCPU->GetBufferPointer(), vertices.data(), vbByteSize);

	ThrowIfFailed(D3DCreateBlob(ibByteSize, &geo->IndexBufferCPU));
	CopyMemory(geo->IndexBufferCPU->GetBufferPointer(), indices.data(), ibByteSize);

	geo->VertexBufferGPU = d3dUtil::CreateDefaultBuffer(md3dDevice.Get(),
		mCommandList.Get(), vertices.data(), vbByteSize, geo->VertexBufferUploader);

	geo->IndexBufferGPU = d3dUtil::CreateDefaultBuffer(md3dDevice.Get(),
		mCommandList.Get(), indices.data(), ibByteSize, geo->IndexBufferUploader);

	geo->VertexByteStride = sizeof(Vertex);
	geo->VertexBufferByteSize = vbByteSize;
	geo->IndexFormat = DXGI_FORMAT_R16_UINT;
	geo->IndexBufferByteSize = ibByteSize;

	geo->DrawArgs["candy"] = candySubmesh;
	geo->DrawArgs["squareBucket"] = squareBucketSubmesh;
	geo->DrawArgs["hexagonBucket"] = hexagonBucketSubmesh;

	mGeometries[geo->Name] = std::move(geo);
}

void LitColumnsApp::BuildPSOs()
{
    D3D12_GRAPHICS_PIPELINE_STATE_DESC opaquePsoDesc;

	//
	// PSO for opaque objects.
	//
    ZeroMemory(&opaquePsoDesc, sizeof(D3D12_GRAPHICS_PIPELINE_STATE_DESC));
	opaquePsoDesc.InputLayout = { mInputLayout.data(), (UINT)mInputLayout.size() };
	opaquePsoDesc.pRootSignature = mRootSignature.Get();
	opaquePsoDesc.VS = 
	{ 
		reinterpret_cast<BYTE*>(mShaders["standardVS"]->GetBufferPointer()), 
		mShaders["standardVS"]->GetBufferSize()
	};
	opaquePsoDesc.PS = 
	{ 
		reinterpret_cast<BYTE*>(mShaders["opaquePS"]->GetBufferPointer()),
		mShaders["opaquePS"]->GetBufferSize()
	};
	opaquePsoDesc.RasterizerState = CD3DX12_RASTERIZER_DESC(D3D12_DEFAULT);
	opaquePsoDesc.BlendState = CD3DX12_BLEND_DESC(D3D12_DEFAULT);
	opaquePsoDesc.DepthStencilState = CD3DX12_DEPTH_STENCIL_DESC(D3D12_DEFAULT);
	opaquePsoDesc.SampleMask = UINT_MAX;
	opaquePsoDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
	opaquePsoDesc.NumRenderTargets = 1;
	opaquePsoDesc.RTVFormats[0] = mBackBufferFormat;
	opaquePsoDesc.SampleDesc.Count = m4xMsaaState ? 4 : 1;
	opaquePsoDesc.SampleDesc.Quality = m4xMsaaState ? (m4xMsaaQuality - 1) : 0;
	opaquePsoDesc.DSVFormat = mDepthStencilFormat;
    ThrowIfFailed(md3dDevice->CreateGraphicsPipelineState(&opaquePsoDesc, IID_PPV_ARGS(&mOpaquePSO)));
}

void LitColumnsApp::BuildFrameResources()
{
    for(int i = 0; i < gNumFrameResources; ++i)
    {
        mFrameResources.push_back(std::make_unique<FrameResource>(md3dDevice.Get(),
            1, (UINT)mAllRitems.size(), (UINT)mMaterials.size()));
    }
}

void LitColumnsApp::BuildMaterials()
{
	auto redCandy = std::make_unique<Material>();
	redCandy->Name = "redCandy";
	redCandy->MatCBIndex = 0;
	redCandy->DiffuseSrvHeapIndex = 0;
	redCandy->DiffuseAlbedo = XMFLOAT4(Colors::Red);
	redCandy->FresnelR0 = XMFLOAT3(0.02f, 0.02f, 0.02f);
	redCandy->Roughness = 0.1f;

	auto blueCandy = std::make_unique<Material>();
	blueCandy->Name = "blueCandy";
	blueCandy->MatCBIndex = 1;
	blueCandy->DiffuseSrvHeapIndex = 1;
	blueCandy->DiffuseAlbedo = XMFLOAT4(Colors::Blue);
	blueCandy->FresnelR0 = XMFLOAT3(0.02f, 0.02f, 0.02f);
	blueCandy->Roughness = 0.1f;

	auto greenCandy = std::make_unique<Material>();
	greenCandy->Name = "greenCandy";
	greenCandy->MatCBIndex = 2;
	greenCandy->DiffuseSrvHeapIndex = 2;
	greenCandy->DiffuseAlbedo = XMFLOAT4(Colors::Green);
	greenCandy->FresnelR0 = XMFLOAT3(0.02f, 0.02f, 0.02f);
	greenCandy->Roughness = 0.1f;

	auto yellowCandy = std::make_unique<Material>();
	yellowCandy->Name = "yellowCandy";
	yellowCandy->MatCBIndex = 3;
	yellowCandy->DiffuseSrvHeapIndex = 3;
	yellowCandy->DiffuseAlbedo = XMFLOAT4(Colors::Yellow);
	yellowCandy->FresnelR0 = XMFLOAT3(0.02f, 0.02f, 0.02f);
	yellowCandy->Roughness = 0.1f;

	auto purpleCandy = std::make_unique<Material>();
	purpleCandy->Name = "purpleCandy";
	purpleCandy->MatCBIndex = 4;
	purpleCandy->DiffuseSrvHeapIndex = 4;
	purpleCandy->DiffuseAlbedo = XMFLOAT4(Colors::Purple);
	purpleCandy->FresnelR0 = XMFLOAT3(0.02f, 0.02f, 0.02f);
	purpleCandy->Roughness = 0.1f;

	auto bucket = std::make_unique<Material>();
	bucket->Name = "bucket";
	bucket->MatCBIndex = 5;
	bucket->DiffuseSrvHeapIndex = 5;
	bucket->DiffuseAlbedo = XMFLOAT4(Colors::Beige);
	bucket->FresnelR0 = XMFLOAT3(0.1f, 0.1f, 0.1f);
	bucket->Roughness = 0.6f;
	
	mMaterials["redCandy"] = std::move(redCandy);
	mMaterials["blueCandy"] = std::move(blueCandy);
	mMaterials["greenCandy"] = std::move(greenCandy);
	mMaterials["yellowCandy"] = std::move(yellowCandy);
	mMaterials["purpleCandy"] = std::move(purpleCandy);
	mMaterials["bucket"] = std::move(bucket);
}

void LitColumnsApp::BuildRenderItems()
{
	FLOAT boxWidth = 30;
	FLOAT boxDepth = 30;
	FLOAT hWidth = boxWidth * 0.5f;
	FLOAT hDepth = boxDepth * 0.5f;

	std::random_device rd;
	std::mt19937 eng(rd());
	std::uniform_real_distribution<> distrX(-hWidth, hWidth);
	std::uniform_real_distribution<> distrZ(-hDepth, hDepth);
	std::uniform_int_distribution<> distrColor(0, 4);

	UINT candyInBucket = 20;
	UINT lightIndex = 1;
	UINT colorIndex = 0;

	UINT candyNum = 100;
	const char* colorNames[] = {
		"redCandy",
		"blueCandy",
		"greenCandy",
		"yellowCandy",
		"purpleCandy"
	};

	XMFLOAT3 colors[] = {
		XMFLOAT3(Colors::Red),
		XMFLOAT3(Colors::Blue),
		XMFLOAT3(Colors::Green),
		XMFLOAT3(Colors::Yellow)
	};

	UINT objCBIndex = 0;
	for (int i = 0; i < candyNum; ++i) {
		auto candyRItem = std::make_unique<RenderItem>();

		FLOAT randX = distrX(eng);
		FLOAT randZ = distrZ(eng);
		const char * curColor = colorNames[i / 20];

		XMStoreFloat4x4(&candyRItem->World, XMMatrixScaling(.5f, .5f, .5f)*XMMatrixTranslation(randX, 0.f, randZ));
		candyRItem->TexTransform = MathHelper::Identity4x4();
		candyRItem->Mat = mMaterials[curColor].get();
		candyRItem->ObjCBIndex = objCBIndex++;
		candyRItem->Geo = mGeometries["shapeGeo"].get();
		candyRItem->PrimitiveType = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
		candyRItem->IndexCount = candyRItem->Geo->DrawArgs["candy"].IndexCount;
		candyRItem->StartIndexLocation = candyRItem->Geo->DrawArgs["candy"].StartIndexLocation;
		candyRItem->BaseVertexLocation = candyRItem->Geo->DrawArgs["candy"].BaseVertexLocation;
		mAllRitems.push_back(std::move(candyRItem));
	}

	for (int i = 0; i < 2; ++i) {
		auto bucketRItem = std::make_unique<RenderItem>();

		FLOAT randX = distrX(eng);
		FLOAT randZ = distrZ(eng);

		XMStoreFloat4x4(&bucketRItem->World, XMMatrixScaling(1.f, 1.f, 1.f) * XMMatrixTranslation(randX, 0.f, randZ));
		bucketRItem->TexTransform = MathHelper::Identity4x4();
		bucketRItem->Mat = mMaterials["bucket"].get();
		bucketRItem->ObjCBIndex = objCBIndex++;
		bucketRItem->Geo = mGeometries["shapeGeo"].get();
		bucketRItem->PrimitiveType = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
		bucketRItem->IndexCount = bucketRItem->Geo->DrawArgs["squareBucket"].IndexCount;
		bucketRItem->StartIndexLocation = bucketRItem->Geo->DrawArgs["squareBucket"].StartIndexLocation;
		bucketRItem->BaseVertexLocation = bucketRItem->Geo->DrawArgs["squareBucket"].BaseVertexLocation;
		mAllRitems.push_back(std::move(bucketRItem));

		std::random_device rdc;
		std::mt19937 engc(rdc());
		std::uniform_real_distribution<> distrcX(randX-1.f, randX+1.f);
		std::uniform_real_distribution<> distrcZ(randZ-1.f, randZ+1.f);

		mMainPassCB.Lights[lightIndex].Strength = colors[colorIndex++];
		mMainPassCB.Lights[lightIndex].Position = XMFLOAT3(randX, 1.2f, randZ);
		mMainPassCB.Lights[lightIndex].SpotPower = 1.f;
		mMainPassCB.Lights[lightIndex++].Direction = XMFLOAT3(0.f, 0.f, 0.f);

		for (int j = 0; j < candyInBucket; ++j) {
			auto candyRItem = std::make_unique<RenderItem>();

			FLOAT randcX = distrcX(engc);
			FLOAT randcZ = distrcZ(engc);

			XMStoreFloat4x4(&candyRItem->World, XMMatrixScaling(.5f, .5f, .5f)*XMMatrixTranslation(randcX, 0.f, randcZ));
			candyRItem->TexTransform = MathHelper::Identity4x4();
			candyRItem->Mat = mMaterials[colorNames[distrColor(eng)]].get();
			candyRItem->ObjCBIndex = objCBIndex++;
			candyRItem->Geo = mGeometries["shapeGeo"].get();
			candyRItem->PrimitiveType = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
			candyRItem->IndexCount = candyRItem->Geo->DrawArgs["candy"].IndexCount;
			candyRItem->StartIndexLocation = candyRItem->Geo->DrawArgs["candy"].StartIndexLocation;
			candyRItem->BaseVertexLocation = candyRItem->Geo->DrawArgs["candy"].BaseVertexLocation;
			mAllRitems.push_back(std::move(candyRItem));
		}
	}

	for (int i = 0; i < 2; ++i) {
		auto bucketRItem = std::make_unique<RenderItem>();

		FLOAT randX = distrX(eng);
		FLOAT randZ = distrZ(eng);

		XMStoreFloat4x4(&bucketRItem->World, XMMatrixScaling(1.f, 1.f, 1.f) * XMMatrixTranslation(randX, 0.f, randZ));
		bucketRItem->TexTransform = MathHelper::Identity4x4();
		bucketRItem->Mat = mMaterials["bucket"].get();
		bucketRItem->ObjCBIndex = objCBIndex++;
		bucketRItem->Geo = mGeometries["shapeGeo"].get();
		bucketRItem->PrimitiveType = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
		bucketRItem->IndexCount = bucketRItem->Geo->DrawArgs["hexagonBucket"].IndexCount;
		bucketRItem->StartIndexLocation = bucketRItem->Geo->DrawArgs["hexagonBucket"].StartIndexLocation;
		bucketRItem->BaseVertexLocation = bucketRItem->Geo->DrawArgs["hexagonBucket"].BaseVertexLocation;
		mAllRitems.push_back(std::move(bucketRItem));

		mMainPassCB.Lights[lightIndex].Strength = colors[colorIndex++];
		mMainPassCB.Lights[lightIndex].Position = XMFLOAT3(randX, 0.5f, randZ);
		mMainPassCB.Lights[lightIndex].SpotPower = 1.f;
		mMainPassCB.Lights[lightIndex++].Direction = XMFLOAT3(0.f, 0.f, 0.f);

		std::random_device rdc;
		std::mt19937 engc(rdc());
		std::uniform_real_distribution<> distrcX(randX - 1.f, randX + 1.f);
		std::uniform_real_distribution<> distrcZ(randZ - 1.f, randZ + 1.f);

		for (int j = 0; j < candyInBucket; ++j) {
			auto candyRItem = std::make_unique<RenderItem>();

			FLOAT randcX = distrcX(engc);
			FLOAT randcZ = distrcZ(engc);

			XMStoreFloat4x4(&candyRItem->World, XMMatrixScaling(.5f, .5f, .5f)*XMMatrixTranslation(randcX, 0.f, randcZ));
			candyRItem->TexTransform = MathHelper::Identity4x4();
			candyRItem->Mat = mMaterials[colorNames[distrColor(eng)]].get();
			candyRItem->ObjCBIndex = objCBIndex++;
			candyRItem->Geo = mGeometries["shapeGeo"].get();
			candyRItem->PrimitiveType = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
			candyRItem->IndexCount = candyRItem->Geo->DrawArgs["candy"].IndexCount;
			candyRItem->StartIndexLocation = candyRItem->Geo->DrawArgs["candy"].StartIndexLocation;
			candyRItem->BaseVertexLocation = candyRItem->Geo->DrawArgs["candy"].BaseVertexLocation;
			mAllRitems.push_back(std::move(candyRItem));
		}
	}

	// All the render items are opaque.
	for(auto& e : mAllRitems)
		mOpaqueRitems.push_back(e.get());
}

void LitColumnsApp::DrawRenderItems(ID3D12GraphicsCommandList* cmdList, const std::vector<RenderItem*>& ritems)
{
    UINT objCBByteSize = d3dUtil::CalcConstantBufferByteSize(sizeof(ObjectConstants));
    UINT matCBByteSize = d3dUtil::CalcConstantBufferByteSize(sizeof(MaterialConstants));
 
	auto objectCB = mCurrFrameResource->ObjectCB->Resource();
	auto matCB = mCurrFrameResource->MaterialCB->Resource();

    // For each render item...
    for(size_t i = 0; i < ritems.size(); ++i)
    {
        auto ri = ritems[i];

        cmdList->IASetVertexBuffers(0, 1, &ri->Geo->VertexBufferView());
        cmdList->IASetIndexBuffer(&ri->Geo->IndexBufferView());
        cmdList->IASetPrimitiveTopology(ri->PrimitiveType);

        D3D12_GPU_VIRTUAL_ADDRESS objCBAddress = objectCB->GetGPUVirtualAddress() + ri->ObjCBIndex*objCBByteSize;
		D3D12_GPU_VIRTUAL_ADDRESS matCBAddress = matCB->GetGPUVirtualAddress() + ri->Mat->MatCBIndex*matCBByteSize;

        cmdList->SetGraphicsRootConstantBufferView(0, objCBAddress);
		cmdList->SetGraphicsRootConstantBufferView(1, matCBAddress);

        cmdList->DrawIndexedInstanced(ri->IndexCount, 1, ri->StartIndexLocation, ri->BaseVertexLocation, 0);
    }
}
